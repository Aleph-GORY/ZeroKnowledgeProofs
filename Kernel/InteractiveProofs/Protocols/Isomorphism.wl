(* ::Package:: *)
(*
  # Isomoorphism protoocol
  Interactive zk-Proof protocol based on Isomorphism NP complete problem
*)

(* ::Section:: *)
(* Cipher algoritms *)

(* 
  Graph functions
*)
  generateGraphIsomorphism[size_] := Thread[
    Range[size] -> RandomSample[Range[size]]
  ]

  IsomorphicGraph[graph_, isomorphism_] := Graph[
    RandomSample@EdgeList@VertexReplace[graph, isomorphism],
    VertexLabels -> Automatic
  ]
  IsomorphicSortedGraph[graph_, isomorphism_] := Graph[
    Sort /@ EdgeList[VertexReplace[graph, isomorphism]], 
    VertexLabels -> Automatic
  ]

  ResetVertexList[graph_] := Module[{},
    vertex = VertexList[graph];
    size = Length@vertex;
    key = Thread[vertex -> RandomSample[Range[size]]];
    {IsomorphicGraph[graph, key],key}
  ]

  GraphXor[G_,H_ ] := Graph[
    RandomSample@SymmetricDifference[EdgeList[G], EdgeList[H]],
    VertexLabels -> Automatic
  ]

(* 
  GXOR Cipher
*)
  GXORProblem[publicProblem_, key_] := Module[
    {
      P1 = publicProblem[[1]],
      P2 = publicProblem[[2]]
    },
    SeedRandom[key];
    size = Length@VertexList[P1];
    (* Cipher *)
    isomorphismCipher = generateGraphIsomorphism[size];
    graphCipher1 = generateGraphIsomorphism[size];
    graphCipher2 = generateGraphIsomorphism[size];
    (* Apply cipher  *)
    cipherP1 = GraphXor[
      IsomorphicGraph[P1, graphCipher1],
      IsomorphicGraph[P1, graphCipher2]
    ];
    cipherP2 = GraphXor[
      IsomorphicGraph[P2, 
        Association[graphCipher1]/@
        Association[isomorphismCipher]//Normal
      ],
      IsomorphicGraph[P2, 
        Association[graphCipher2]/@
        Association[isomorphismCipher]//Normal
      ]
    ];
    (* Return CipherProblem *)
    {cipherP1, cipherP2}
  ]

  GXORSolution[privateSolution_, key_] := Module[{},
    SeedRandom[key];
    size = Length@privateSolution;
    (* Cipher *)
    isomorphismCipher = generateGraphIsomorphism[size];
    (* Apply cipher  *)
    Association[isomorphismCipher] /@
    Association[privateSolution] // Normal
  ]

  GXOR[privateSolution_, key_] := Module[
    {
      solution = privateSolution["PrivateSolution"],
      problem = privateSolution["PublicProblem"]
    },
    {
      GXORProblem[problem, key],
      <|
        "CipherSolution" -> GXORSolution[solution, key],
        "CipherKey" -> key
      |>
    }
  ]

  GXORCipher = <|
    "Name" -> "GXOR",
    "Cipher" -> GXOR,
    "ProblemCipher" -> GXORProblem,
    "CipherSolutionShape" -> "List of isomorphisms between GXOR graphs.",
    "CipherProblemShape" -> "List of pairs of homomorphic GXOR graphs."
  |>

  CipherTransformation["Isomorphism"] = GXORCipher


(* ::Section:: *)
(* Protocol definition *)

(* 
  GenerateZeroKnowledgePrivateSolution
*)
  GenerateZeroKnowledgePrivateSolution["Isomorphism", opts: OptionsPattern[{Seed->Null, Size->64}]] := Module[{},
    seed = OptionValue[Seed];
    key = If[seed === Null, Hash[RandomReal[], "SHA"], seed];
    size = OptionValue[Size];
    SeedRandom[key];

    graph=IsomorphicGraph[
      RandomGraph[{size, 8*size}],
      Thread[Range[size] -> Range[size]]
    ];
    isomorphism = generateGraphIsomorphism[size];
    ZeroKnowledgePrivateSolution[<|
      "Protocol" -> "Isomorphism",
      "PrivateSolutionShape" -> "Isomorphism betweeen two graphs.",
      "PrivateSolutionSize" -> size,
      "PublicProblemShape" -> "A pair of isomorphic graphs.",
      "PublicProblemSize" -> {size, 4*size},
      "PrivateSolution" -> isomorphism,
      "PublicProblem" -> {graph, IsomorphicGraph[graph, isomorphism]}
    |>]
  ]

(* 
  CipherPrivateSolution
*)
  CipherPrivateSolution["Isomorphism", privateSolution_, opts:OptionsPattern[{WitnessSize->5}]] := Module[
    {
      cipher = CipherTransformation["Isomorphism"]
    },
    cipherSolution = Table[
      cipher["Cipher"][
        privateSolution, Hash[RandomReal[], "SHA"]
      ], OptionValue[WitnessSize]
    ];
    <|
      "Protocol" -> privateSolution["Protocol"],
      "CipherTransformation" -> cipher["Name"],
      "CipherSolutionShape" -> cipher["CipherSolutionShape"],
      "CipherProblemShape" -> cipher["CipherProblemShape"],

      "PublicCipherProblem" -> First/@cipherSolution,
      "PrivateCipherSolution" -> Last/@cipherSolution
    |>
  ]

(* 
  VerifyZeroKnowledgeProof
*)
  VerifyZeroKnowledgeResponse["Isomorphism", publicProblem_, cipherProblem_, 0, isomorphism_] := Module[{},
    size = Length[isomorphism];
    G = IsomorphicSortedGraph[First[cipherProblem], isomorphism];
    H = IsomorphicSortedGraph[Last[cipherProblem], Thread[Range[size] -> Range[size]]];
    G == H
  ]
  VerifyZeroKnowledgeResponse["Isomorphism", publicProblem_, cipherProblem_, 1, key_] := Module[
    {
      cipher = CipherTransformation["Isomorphism"]
    },
    cipher["ProblemCipher"][publicProblem, key] == cipherProblem
  ]

  VerifyZeroKnowledgeProof["Isomorphism", publicProblem_, witness_, opts:OptionsPattern[{query->Null, response->Null}]] :=
    VerifyInteractiveProof[
      publicProblem, 
      witness,
      OptionValue[query],
      OptionValue[response]
    ]