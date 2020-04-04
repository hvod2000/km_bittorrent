BencodeDecodeInteger[code_] := Module[{result, endl},
    endl = ToCharacterCode["e"][[1]];
    result = TakeWhile[code, # != endl &];
    {result, Drop[code, Length@result+1]}
];

BencodeDecodeString[code_] := Module[{length, len, rest},
    endl = ToCharacterCode[":"][[1]];
    length = TakeWhile[code, # != endl &];
    rest = Drop[code, Length@length+1];
    len = ToExpression@ExportString[length, "UnsignedInteger8"];
    {Take[rest, len], Drop[rest, len]}
];


BencodeDecodeList[code_] := Module[{result, elem, char, data},
    result = {};
    data = code;
    char = First@data;
    While [char != First@ToCharacterCode@"e",
        {elem, data} = BencodeDecode[data];
        char = First@data;
        result = Append[result, elem];
    ];
    {result, Rest@data}
];

BencodeDecodeDict[code_] := Module[{result, key, elem, char, data},
    result = <| |>;
    data = code;
    char = First@data;
    While [char != First@ToCharacterCode@"e",
        {key, data} = BencodeDecodeString[data];
        {elem, data} = BencodeDecode[data];
        key = ExportString[key, "UnsignedInteger8"];
        result = Append[result, key -> elem ];
        char = First@data;
    ];
    {result, Rest@data}
];

BencodeDecode[code_] := Module[{char},
    char = First@code;
    Switch [char,
        First@ToCharacterCode@"i", BencodeDecodeInteger@Rest@code,
        First@ToCharacterCode@"l", BencodeDecodeList@Rest@code,
        First@ToCharacterCode@"d", BencodeDecodeDict@Rest@code,
        _, BencodeDecodeString@code
    ]
];

BencodeLoad[path_] := First@BencodeDecode@Import[path, "UnsignedInteger8"]
