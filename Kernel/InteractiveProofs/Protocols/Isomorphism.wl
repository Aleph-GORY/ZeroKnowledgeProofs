(* ::Package:: *)
(*
  # Isomoorphism protoocol
  Interactive zk-Proof protocol based on Isomorphism NP complete problem
*)


(*
  Helper functions
*)
  generateGraphIsomorphism[size_] := 
    Thread[Range[size] -> RandomSample[Range[size]]]
  generateSATIsomorphism[size_] := 
    Thread[Range[size] -> RandomSample[Range[size]]]

  IsomorphicGraph[graph_, isomorphism_] := 
    Graph[
      EdgeList[VertexReplace[graph, isomorphism]], 
      VertexLabels -> Automatic
    ]

  IsomorphicSortedGraph[graph_, isomorphism_] := 
    Graph[
      Sort /@ EdgeList[VertexReplace[graph, isomorphism]], 
      VertexLabels -> Automatic
    ]
  IsomorphicHamiltonianCycle[cycle_, isomorphism_] := 
    Sort /@ Thread[
      UndirectedEdge[Association[isomorphism] /@ First /@ cycle, 
      Association[isomorphism] /@ Last /@ cycle]
    ]
  IsomorphicBooleanFunction[boolf_, isomorphism_] := 
    With[{permutation = Lookup[isomorphism, Range[Length[isomorphism]]]}, boolf @@ #[[permutation]] &]
  IsomorphicBooleanVector[vector_, isomorphism_] := 
    Array[vector[[isomorphism[#]]] &, Length[vector]]

(* 
  GenerateZeroKnowledgePrivateSolution
*)
  GenerateZeroKnowledgePrivateSolution["Isomorphism", keySize_ : 64] := 
    Module[
      { graph = IsomorphicGraph[RandomGraph[{keySize, 4*keySize}], Thread[Range[keySize] -> Range[keySize]]], 
      isomorphism = generateGraphIsomorphism[keySize]},
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
  CipherPrivateSolution["Isomorphism", privateSolution_] := 
    Module[
      {cipher = generateGraphIsomorphism[privateSolution["PrivateSolutionSize"]]},
      <|
        "PrivateCipherSolution" -> <|
          "Cipher" -> cipher, 
          "Solution" -> Association[privateSolution["PrivateSolution"]] /@ 
            Association[cipher] // Normal
        |>,
        "PublicCipherProblem" -> IsomorphicGraph[
          privateSolution["PublicProblem"] // First, Reverse[cipher, {2}]
        ]
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

