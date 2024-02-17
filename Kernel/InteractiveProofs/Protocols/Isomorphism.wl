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
      EdgeList[VertexReplace[graph, isomorphism]], 
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
  HG2G2[privateSolution_] := privateSolution

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

