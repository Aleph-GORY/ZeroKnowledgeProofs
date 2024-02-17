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

  ResetVertexList[graph_] := Module[{},
    vertex = VertexList[graph];
    size = Length@vertex;
    key = Thread[vertex -> RandomSample[Range[size]]];
    {IsomorphicGraph[graph, key],key}
  ]

(* 
  GXOR Cipher
*)
  GXORProblem[publicProblem_, key_] := Module[
    {
      G = publicProblem[[1]],
      H = publicProblem[[2]]
    },
    SeedRandom[key];
    size = Length@VertexList[G];
    (* Noise *)
    isomorphismNoise1 = generateGraphIsomorphism[size];
    isomorphismNoise2 = generateGraphIsomorphism[size];
    graphNoise = EdgeList@IsomorphicGraph[
      RandomGraph[{size, 8*size}],
      Thread[Range[size] -> Range[size]]
    ];
    (* Add the noise  *)
    noiseG = IsomorphicGraph[Graph[
      RandomSample@SymmetricDifference[EdgeList[G], graphNoise],
      VertexLabels -> Automatic
    ], isomorphismNoise1];
    noiseH = IsomorphicGraph[Graph[
      RandomSample@SymmetricDifference[EdgeList[H], graphNoise],
      VertexLabels -> Automatic
    ], isomorphismNoise2];
    (* Return CipherProblem *)
    {noiseG, noiseH}
  ]

  GXORSolution[privateSolution_, key_] := Module[{},
    SeedRandom[key];
    size = Length@privateSolution;
    (* Noise *)
    isomorphismNoise1 = generateGraphIsomorphism[size];
    isomorphismNoise2 = generateGraphIsomorphism[size];
    (* Add the noise  *)
    Association[Reverse[isomorphismNoise1,{2}]] /@
    Association[privateSolution] /@
    Association[isomorphismNoise2]
  ]

  GXOR[privateSolution_, key_] := Module[
    {
      solution = privateSolution["PrivateSolution"],
      problem = privateSolution["PublicProblem"]
    },
    <|
      "problem" -> GXORProblem[problem, key],
      "solution" -> GXORSolution[solution, key],
      "key" -> key
    |>
  ]

  GXORCipher = <|
    "Name" -> "GXOR",
    "Cipher" -> GXOR,
    "CipherSolutionShape" -> "List of isomorphisms between HG2G2 graphs.",
    "CipherProblemShape" -> "List of pairs of homomorphic G2G2 graphs."
  |>

  CipherTransformation["Isomorphism"] = GXORCipher


(* ::Section:: *)
(* Protocol definition *)

(* 
  GenerateZeroKnowledgePrivateSolution
*)
  GenerateZeroKnowledgePrivateSolution["Isomorphism", Null] := GenerateZeroKnowledgePrivateSolution["Isomorphism"]

  GenerateZeroKnowledgePrivateSolution["Isomorphism", keySize_ : 64] := Module[
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
  CipherPrivateSolution["Isomorphism", privateSolution_, opts:OptionsPattern[{Size->5}]] := Module[
    {
      cipher = CipherTransformation["Isomorphism"],
      cipherSolution = Table[
        cipher["Cipher"][
          privateSolution,
          Hash[RandomReal[], "SHA"]
        ], 
        OptionValue[Size]
      ]
    },
    <|
      "Protocol" -> privateSolution["Protocol"],
      "CipherTransformation" -> cipher["Name"],
      "CipherSolutionShape" -> cipher["CipherSolutionShape"],
      "CipherProblemShape" -> cipher["CipherProblemShape"],

      "PublicCipherProblem" -> #["problem"]&/@cipherSolution,
      "PrivateCipherSolution" -> #["solution"]&/@cipherSolution,
      "PrivateCipherKey" -> #["key"]&/@cipherSolution,
    |>
  ]

(* 
  VerifyZeroKnowledgeResponse
*)
  VerifyZeroKnowledgeResponse["Isomorphism", publicProblem_, cipherProblem_, "Cipher", response_] := 
    IsomorphicGraph[cipherProblem, response] == (publicProblem // First)
  VerifyZeroKnowledgeResponse["Isomorphism", publicProblem_, cipherProblem_, "Solution", response_] := 
    IsomorphicGraph[cipherProblem, response] == (publicProblem // Last)

