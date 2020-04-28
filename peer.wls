#!/usr/bin/env wolframscript

Handshake[] := 
{
	19, ToCharacterCode["BitTorrent protocol"], 
	0, 0, 0, 0, 0, 0, 0, 0,
	Normal[infoHash],
	ToCharacterCode[peerId]
} //Flatten

ClearAll[Peer]

PeerConnect[adress_] := 
Block[{socket},
	socket = BufferedSocketConnect[adress, "TCP"];
	socket["SendBytes", Handshake[]];
	Peer[Null, socket]
]

Process[p:Peer[Null, socket_BufferedSocket]] ^:=
If[socket@"IsReady",
	Block[{msg},
		msg = socket["ReadBytes", 68];
		If[ByteArray@msg[[29;;48]] === infoHash,
			Peer[
				{0, 1, 0, 1}, 
				socket,
				ConstantArray[0, piecesCount]
			],
			Null
		]
	],
	p
]

Process[p:Peer[state_List, socket_BufferedSocket, pieces_List]] ^:= 
Block[{
	amChoking, amInterested, peerChoking, peerInterested,
	newPieces = pieces,
	msgLen, msgID
	},
	{amChoking, amInterested, peerChoking, peerInterested}= state;
	While[socket["IsReady"],
		msgLen = FromDigits[socket["ReadBytes", 4], 256];
		If[msgLen === 0, Continue[]];(*keep-alive*)
		msgID = socket["ReadBytes", 1] // First;
		Switch[msgID,
		0, peerChoking = 1,
		1, peerChoking = 0,
		2, peerInterested = 1,
		3, peerInterested = 0,
		4, newPieces[[FromDigits[socket["ReadBytes", 4], 256]]] = 1,
		5, newPieces = 
			IntegerDigits[#, 2]& /@ socket["ReadBytes", msgLen - 1] //Flatten,
		7, AppendTo[pieces, socket["ReadBytes", msgLen - 1]]
		]
	];
	Peer[{amChoking, amInterested, peerChoking, peerInterested},
		  socket,
		  newPieces]
]