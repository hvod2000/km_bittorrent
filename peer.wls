#!/usr/bin/env wolframscript

ClearAll[Peer]
SetAttributes[Peer, HoldAll]

Handshake[] := 
{
	19, ToCharacterCode["BitTorrent protocol"], 
	0, 0, 0, 0, 0, 0, 0, 0,
	Normal[infoHash],
	ToCharacterCode[peerId]
} //Flatten

PeerConnect[adress_] := 
Module[{self = <|
	"socket" -> BufferedSocketConnect[adress, "TCP"],
	"state" -> "handshaking",
	"amChoking" -> 1,
	"amInterested" -> 0,
	"peerChoking" -> 1,
	"peerInterested" -> 0,
	"pieces" -> ConstantArray[0, piecesCount]
|>},
	self["socket"]["SendBytes", Handshake[]];
	Peer[self]
]

Peer[self_]["ReadMessages"]:= 
Block[{msgLen, msgID},
	If[self@"state" === "handshaking" && self["socket"]["IsReady"],
		If[self["socket"]["ReadBytes", 68][[29;;48]] === Normal@infoHash,
			self@"state" = "alive",
			self@"state" = "dead"]
	];
	While[self@"state" === "alive" && self["socket"]["IsReady"],
		msgLen = FromDigits[self["socket"]["ReadBytes", 4], 256];
		If[msgLen === 0, Continue[]];(*keep-alive message*)
		msgID = self["socket"]["ReadBytes", 1] // First;
		Switch[msgID,
		0, self@"peerChoking" = 1,
		1, self@"peerChoking" = 0,
		2, self@"peerInterested" = 1,
		3, self@"peerInterested" = 0,
		4, self[["pieces", FromDigits[self["socket"]["ReadBytes", 4], 256]]] = 1,
		5, self@"pieces" = 
			IntegerDigits[#, 2]& /@ self["socket"]["ReadBytes", msgLen - 1] //Flatten,
		6,,
		7, AppendTo[tmpDownloaded, self["socket"]["ReadBytes", msgLen - 1]]
		]
	];
]

Peer[self_]["Get", property_String]:= self[property]

Peer[self_]["Send", "choke"]:= (
	self["socket"]["SendBytes", {0,0,0,1,0}];
	self@"amChoking" = 1;)
	
Peer[self_]["Send", "unchoke"]:= (
	self["socket"]["SendBytes", {0,0,0,1,1}];
	self@"amChoking" = 0;)
	
Peer[self_]["Send", "interested"]:= (
	self["socket"]["SendBytes", {0,0,0,1,2}];
	self@"amInterested" = 1;)
	
Peer[self_]["Send", "not interested"]:= (
	self["socket"]["SendBytes", {0,0,0,1,3}];
	self@"amInterested" = 0;)
	
Peer[self_]["Send", "have", index_Integer]:= 
	(self["socket"]["SendBytes", 
		{0,0,0,5,4, IntegerDigits[index, 256, 4]} // Flatten
	];)

Peer[self_]["Send", "request", index_Integer, begin_Integer, length_Integer]:= 
	(self["socket"]["SendBytes", 
		{0,0,0,13,6,
		IntegerDigits[index, 256, 4],
		IntegerDigits[begin, 256, 4],
		IntegerDigits[length, 256, 4]
		} // Flatten
	];)