    #!/usr/bin/env wolframscript
Import["bencode-encode.wls"]
Import["bencode-decode.wls"]

CreatePeerId[] :=
    "-KM001-" <> StringJoin@Array[ToString@RandomInteger[9] &, 13]

TorrentTracker[path_String, infoHash_ByteArray, size_Integer, port_Integer] :=
    TorrentTracker[CreatePeerId[], path, infoHash, size, port]

TorrentTracker[peerId_String, path_String,
               infoHash_ByteArray, size_Integer, port_Integer] :=
    TorrentTracker[path, {
        "info_hash" -> ByteArrayToString[infoHash, "koi8-r"],
        "peer_id" -> peerId,
        "uploaded" -> "0",
        "downloaded" -> ToString@size,
        "left" -> ToString[size],
        "port" -> ToString[port],
        "compact" -> "1"
    }]

GetPeers[TorrentTracker[path_String, params_List]] ^:=
Block[{url},
    url = URLBuild[path, params, CharacterEncoding -> "koi8-r"];
    BencodeDecode[KmHTTPRequest[url]][["peers"]] // ParsePeers
]

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
    msg = Normal@SocketReadMessage[socket];
    msg /. {___,13, 10, 13, 10,a__}:>{a}
]

ParsePeers[peers_ByteArray] := {
    StringRiffle[ToString /@ Drop[#, -2], "."], Drop[#, 4].{256, 1}
} & /@ Partition[Normal@peers, 6]
