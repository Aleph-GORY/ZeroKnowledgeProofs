(* ::Package:: *)
(*
  # Isomoorphism protoocol
  Interactive zk-Proof protocol based on Isomorphism NP complete problem
*)


(* 
  GenerateZeroKnowledgePrivateSolution
*)
  generateGraphIsomorphism[size_] := 
    Thread[Range[size] -> RandomSample[Range[size]]]

  IsomorphicGraph[graph_, isomorphism_] := 
    Graph[
      RandomSample@EdgeList@VertexReplace[graph, isomorphism],
      VertexLabels -> Automatic
    ]

  GenerateZeroKnowledgePrivateSolution["Isomorphism", Null] := 
    GenerateZeroKnowledgePrivateSolution["Isomorphism"]

  GenerateZeroKnowledgePrivateSolution["Isomorphism", keySize_ : 64] := 
    Module[
      { 
        graph = IsomorphicGraph[
          RandomGraph[{keySize, 8*keySize}],
          Thread[Range[keySize] -> Range[keySize]]
        ],
        isomorphism = generateGraphIsomorphism[keySize]
      },
      ZeroKnowledgePrivateSolution[<|
        "Protocol" -> "Isomorphism",
        "PrivateSolutionShape" -> "Isomorphism betweeen two graphs.",
        "PrivateSolutionSize" -> keySize,
        "PublicProblemShape" -> "A pair of isomorphic graphs.",
        "PublicProblemSize" -> {keySize, 4*keySize},
        "PrivateSolution" -> isomorphism,
        "PublicProblem" -> {graph, IsomorphicGraph[graph, isomorphism]}
      |>]
    ]

(* 
  CipherPrivateSolution
*)
  ResetVertexList[graph_] := Module[{},
    vertex = VertexList[graph];
    size = Length@vertex;
    IsomorphicGraph[graph, Thread[vertex -> RandomSample[Range[size]]]]
  ]

  MergeGraphVertex[graph_] := Module[{},
    size = Length@VertexList[graph];
    ResetVertexList@VertexContract[graph,Partition[RandomSample[Range[size]], 2]]
  ]

  DeleteGraphVertex[graph_] := Module[{},
    vertex = VertexList[graph];
    size = Length@vertex;
    ResetVertexList@VertexDelete[graph,RandomSample[vertex, size/2]]
  ]

  HG2G2[privateSolution_, key_] := Module[
    {
      G = privateSolution[""]
    },
    SeedRandom[key];
    size = Length@VertexList[G];
    (* G2 construction *)
    G2 = DeleteGraphVertex@MergeGraphVertex[G];
    (* H2 construction *)
    H2 = MergeGraphVertex@DeleteGraphVertex[G];
    (* G2G2 construction *)
    G2G2 = DeleteGraphVertex@DeleteGraphVertex[
      GraphProduct[G2,H2,"Conormal"]
    ];
    {G2,H2,G2G2}
  ]

  HG2G2Cipher = <|
    "Name" -> "H-G2G2",
    "Cipher" -> HG2G2,
    "CipherSolutionShape" -> "List of isomorphisms between HG2G2 graphs.",
    "CipherProblemShape" -> "List of pairs of homomorphic G2G2 graphs."
  |>

  CipherTransformation["Isomorphism"] = HG2G2Cipher

  CipherPrivateSolution["Isomorphism", privateSolution_, opts:OptionsPattern[{Size->5}]] := 
    Module[
      {
        cipher = CipherTransformation["Isomorphism"],
        cipherSolution = Table[HG2G2[privateSolution], OptionValue[Size]]
      },
      <|
        "Protocol" -> privateSolution["Protocol"],
        "CipherTransformation" -> cipher["Name"],
        "CipherSolutionShape" -> cipher["CipherSolutionShape"],
        "CipherProblemShape" -> cipher["CipherProblemShape"],

        "PublicCipherProblem" -> First/@cipherSolution,
        "PrivateCipherSolution" -> First/@cipherSolution
      |>
    ]

(*
  GenerateZeroKnowledgeQuery
*)
  ValidQueryList["Isomorphism"] := {"Cipher", "Solution"}
  GenerateZeroKnowledgeQuery[publicWitness_] := 
    ZeroKnowledgeQuery[<|
      "Protocol" -> publicWitness["Protocol"],
      "Query" -> Table[
        RandomChoice[ValidQueryList[publicWitness["Protocol"]]], 
        publicWitness["QuerySize"]
      ]
    |>]

(* 
  VerifyZeroKnowledgeResponse
*)
  VerifyZeroKnowledgeResponse["Isomorphism", publicProblem_, cipherProblem_, "Cipher", response_] := 
    IsomorphicGraph[cipherProblem, response] == (publicProblem // First)
  VerifyZeroKnowledgeResponse["Isomorphism", publicProblem_, cipherProblem_, "Solution", response_] := 
    IsomorphicGraph[cipherProblem, response] == (publicProblem // Last)

