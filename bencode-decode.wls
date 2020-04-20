BencodeDecodeInteger[code_] := Block[{result, endl},
    endl = ToCharacterCode["e"][[1]];
    result = TakeWhile[code, # != endl &];
    {ToExpression@FromCharacterCode@result, Drop[code, Length@result+1]}
];

BencodeDecodeString[code_, encoding_String] := Block[{length, len, rest, str},
    endl = ToCharacterCode[":"][[1]];
    length = TakeWhile[code, # != endl &];
    rest = Drop[code, Length@length+1];
    len = ToExpression@ExportString[length, "Byte"];
    str = Take[rest, len];
    str = Quiet@Check[FromCharacterCode[str, encoding], ByteArray@str];
    {str, Drop[rest, len]}
];

BencodeDecodeList[code_, encoding_String] := Block[{result, elem, char, data},
    result = {};
    data = code;
    char = First@data;
    While [char != First@ToCharacterCode@"e",
        {elem, data} = BencodeDecode[data, encoding];
        char = First@data;
        result = Append[result, elem];
    ];
    {result, Rest@data}
];

BencodeDecodeDict[code_, encoding_String] := Block[{result, key, elem, char, data},
    result = <| |>;
    data = code;
    char = First@data;
    While [char != First@ToCharacterCode@"e",
        {key, data} = BencodeDecodeString[data, encoding];
        {elem, data} = BencodeDecode[data, encoding];
        (*key = ExportString[key, "Byte"];*)
        result = Append[result, key -> elem ];
        char = First@data;
    ];
    {result, Rest@data}
];

BencodeDecode[code_List, encoding_String] := Block[{char},
    char = First@code;
    Switch [char,
        First@ToCharacterCode@"i", BencodeDecodeInteger@Rest@code,
        First@ToCharacterCode@"l", BencodeDecodeList[Rest@code, encoding],
        First@ToCharacterCode@"d", BencodeDecodeDict[Rest@code, encoding],
        _, BencodeDecodeString[code, encoding]
    ]
];

BencodeDecode[code_String, encoding_String] :=
    First@BencodeDecode[ToCharacterCode[code, encoding], encoding]

BencodeDecode[code_ByteArray, encoding_String] :=
    First@BencodeDecode[Normal@code, encoding]

BencodeDecode[code_] :=
    BencodeDecode[code, "UTF8"]

BencodeLoad[path_, encoding_String] := First@BencodeDecode[
    Import[path, "Byte"],
    encoding
]

BencodeLoad[path_] := BencodeLoad[path, "UTF-8"]
