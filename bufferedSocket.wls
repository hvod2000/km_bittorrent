#!/usr/bin/env wolframscript

ClearAll[BufferedSocket, BufferedSocketConnect]
SetAttributes[BufferedSocket, HoldAll]

BufferedSocketConnect[adress__]:=
Module[{self=<|
	"socket"->SocketConnect[adress],
	"buffer"->{},
	"bufferLen"->0,
	"cursor"->1
|>},
	BufferedSocket[self]
]

BufferedSocket[self_]["IsReady"]:=
	(self@"bufferLen" >= self@"cursor" || SocketReadyQ[self@"socket"])
	
BufferedSocket[self_]["FillBuffer"]:= (
	self@"buffer"= If[self@"cursor" > self@"bufferLen",
		{},
		self[["buffer",self@"cursor";;All]]
	];
	self@"buffer" = Join[self@"buffer", Normal@SocketReadMessage[self@"socket"]];
	self@"cursor" = 1;
	self@"bufferLen" = Length[self@"buffer"];
)

(b:BufferedSocket[self_])["ReadBytes", n_Integer]:=
Block[{out},
	If[self@"cursor" + n - 1 > self@"bufferLen", b@"FillBuffer"];
	out = self[["buffer", self["cursor"];;self["cursor"] + n - 1]];
	self@"cursor" += n;
	out
]

BufferedSocket[self_]["SendBytes", data_List]:=
	BinaryWrite[self@"socket", data]