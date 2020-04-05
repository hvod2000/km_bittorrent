#!/usr/bin/env wolframscript
Import["bencode-encode.wls"]
Import["bencode-decode.wls"]

CreatePeerId[] :=
    "-KM001-" <> StringJoin@Array[ToString@RandomInteger[9] &, 12]

TorrentTraker[path_String, infoHash_ByteArray, size_Integer, port_Integer] :=
    TorrentTraker[path, {
        "info_hash" -> ByteArrayToString[infoHash, "koi8-r"],
        "peer_id" -> CreatePeerId[],
        "uploaded" -> "0",
        "downloaded" -> "156",
        "left" -> ToString[size],
        "port" -> ToString[port],
        "compact" -> "1"
    }]

GetPeers[TorrentTraker[path_String, params_List]] ^:=
    ParsePeers[BencodeDecode[KmHTTPRequest[URLBuild[
        path, params, CharacterEncoding -> "koi8-r"
    ]]][["peers"]]]

KmHTTPRequest[url_String] := 
Block[{socket, urll, domain, path, msg, pos},
    urll = URLParse[url, CharacterEncoding -> "koi8-r"];
    domain = urll[["Domain"]];
    path = URLBuild[urll[["Path"]], urll[["Query"]],
        CharacterEncoding -> "koi8-r"];
    socket = SocketConnect["http://" <> domain];
    WriteString[
        socket,
        "GET " <> path <> " HTTP/1.1\r\n" <>
        "Host: " <> domain <> "\r\n" <>
        "User-Agent: uTorrent/355(111915526)(45574)\r\n" <>
        "Accept: text/html\r\n" <>
        "Connection: close\r\n\r\n"
    ];
    msg = ByteArrayToString[SocketReadMessage[socket], "koi8-r"];
    pos = Last@Flatten@StringPosition[msg, Repeated[("\r\n" | "\n"), {2}], 1];
    StringDrop[msg, pos]
]

ParsePeers[peers_String] := {
    StringRiffle[ToString /@ #[[1 ;; 4]], "."],
    #[[5 ;; 6]].{256, 1}
} & /@ Partition[ToCharacterCode[peers, "koi8-r"], 6]
