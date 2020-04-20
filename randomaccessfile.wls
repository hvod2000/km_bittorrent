kmFileWrapper[file_, size_][write[number_, array_]] ^:= Block[{},
    file@seek[size*number];
    file@write[array];
    None
];

kmFileWrapper[file_, size_][close[]] ^:=
    file@close[];

RandomAccessFile[path_, chunksize_] :=
    kmFileWrapper[
    	JavaNew["java.io.RandomAccessFile", path, "rw"],
        chunksize
    ];
