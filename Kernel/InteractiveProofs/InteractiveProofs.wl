(* ::Package:: *)
(* 
  # Interactive zkProofs
  Implementation of interactive zero knowledge proofs
*)

<<"ArmandoCruz`ZeroKnowledgeProofs`InteractiveProofs`Protocols`Isomorphism`"

(* ::Section:: *)
(*Definitions*)

(*
  GenerateZeroKnowledgeQuery
*)
  GenerateZeroKnowledgeQuery[cipherProblem_] := ZeroKnowledgeQuery[
    <|
      "Protocol" -> cipherProblem["Protocol"],
      "Query" -> Table[RandomInteger[], cipherProblem["CipherProblemSize"]],
      "QuerySize" -> cipherProblem["CipherProblemSize"]
    |>
  ]

(*
  AnswerZeroKnowledgeQuery
*)
  AnswerZeroKnowledgeQuery[cipherSolution_, query_] := ZeroKnowledgeResponse[
    <|
      "Protocol" -> cipherSolution["Protocol"],
      "ResponseSize" -> query["QuerySize"],
      "Query" -> query["Query"],
      "Response" -> Array[
        cipherSolution["PrivateCipherSolution"][[#]][[
          query["Query"][[#]]+1
        ]]&, 
        query["QuerySize"]
      ]
    |>
  ]

(*
  VerifyInteractiveProof
*)
  VerifyInteractiveProof[publicProblem_, witness_, query_, response_] := Array[
    Quiet@Check[
      VerifyZeroKnowledgeResponse[
        publicProblem["Protocol"], 
        publicProblem["PublicProblem"], 
        witness["PublicCipherProblems"][[#]], 
        query["Query"][[#]], 
        response["Response"][[#]]
      ] &,
      False
    ],
    query["QuerySize"]
  ]


