namespace FunProbe

open B2R2

open System.Collections.Generic

type DomInfo = {
  DFNumMap: Dictionary<int, int>
  Vertex: int []
  Label: int []
  Parent: int []
  Child: int []
  Ancestor: int []
  Semi: int []
  Bucket: Set<int> []
  Size: int []
  IDom: int []
  MaxLength: int
}

module Dominator =

  let initDomInfo core =
    let len = Array.length core.BasicBlocks + 1
    { DFNumMap = Dictionary ()
      Vertex = Array.zeroCreate len
      Label = Array.create len 0
      Parent = Array.create len 0
      Child = Array.create len 0
      Ancestor = Array.create len 0
      Semi = Array.create len 0
      Bucket = Array.create len Set.empty
      Size = Array.create len 1
      IDom = Array.create len 0
      MaxLength = len }

  let inline dfnum info v =
    info.DFNumMap[v]

  let rec assignDFNum core info n = function
    | (p, v) :: stack when not <| info.DFNumMap.ContainsKey v ->
      info.DFNumMap[v] <- n
      info.Semi[n] <- n
      info.Vertex[n] <- v
      info.Label[n] <- n
      info.Parent[n] <- p
      if v = -1 then
        core.Preds
        |> Array.fold (fun (idx, acc) ps ->
          if List.isEmpty ps then idx + 1, (n, idx) :: acc else idx + 1, acc) (0, stack)
        |> snd
        |> assignDFNum core info (n+1)
      else
        core.Succs[v]
        |> List.fold (fun acc s -> (n, s) :: acc) stack
        |> assignDFNum core info (n+1)
    | _ :: stack -> assignDFNum core info n stack
    | [] ->
      if n = info.MaxLength then n - 1
      else
        let unassigned =
          core.BasicBlocks
          |> Array.fold (fun acc bbl ->
            let idx = bbl.BBLIdx
            if not <| info.DFNumMap.ContainsKey idx then Set.add idx acc
            else acc) Set.empty
        let notCallTargets =
          unassigned
          |> Set.filter (fun idx ->
            let bbl = core.BasicBlocks[idx]
            not <| core.CallTargets.Contains bbl.BBLAddr)
        if Set.count notCallTargets <> 0 then
          assignDFNum core info n [ (0, Set.minElement notCallTargets) ]
        else
          assignDFNum core info n [ (0, Set.minElement unassigned) ]

  let rec compress info v =
    let a = info.Ancestor[v]
    if info.Ancestor[a] <> 0 then
      compress info a
      if info.Semi[info.Label[a]] < info.Semi[info.Label[v]] then
        info.Label[v] <- info.Label[a]
      else ()
      info.Ancestor[v] <- info.Ancestor[a]

  let eval info v =
    if info.Ancestor[v] = 0 then info.Label[v]
    else
      compress info v
      if info.Semi[info.Label[info.Ancestor[v]]] >= info.Semi[info.Label[v]]
      then info.Label[v]
      else info.Label[info.Ancestor[v]]

  /// Compute semidominator of v.
  let rec computeSemiDom info v = function
    | pred :: preds ->
      let u = eval info pred
      if info.Semi[u] < info.Semi[v] then info.Semi[v] <- info.Semi[u]
      computeSemiDom info v preds
    | [] -> ()

  let link info v w =
    let mutable s = w
    while info.Semi[info.Label[w]] < info.Semi[info.Label[info.Child[s]]] do
      if info.Size[s] + info.Size[info.Child[info.Child[s]]]
         >= 2 * info.Size[info.Child[s]]
      then info.Ancestor[info.Child[s]] <- s
           info.Child[s] <- info.Child[info.Child[s]]
      else info.Size[info.Child[s]] <- info.Size[s]
           info.Ancestor[s] <- info.Child[s]
           s <- info.Ancestor[s]
    done
    info.Label[s] <- info.Label[w]
    info.Size[v] <- info.Size[v] + info.Size[w]
    if info.Size[v] < 2 * info.Size[w] then
      let t = s
      s <- info.Child[v]
      info.Child[v] <- t
    while s <> 0 do
      info.Ancestor[s] <- v
      s <- info.Child[s]
    done

  let computeDom info p =
    Set.iter (fun v ->
      let u = eval info v
      if info.Semi[u] < info.Semi[v] then info.IDom[v] <- u
      else info.IDom[v] <- p) info.Bucket[p]
    info.Bucket[p] <- Set.empty

  let rec computeDomOrDelay info parent =
    if info.Bucket[parent].IsEmpty then ()
    else computeDom info parent

  let initDominator core =
    let info = initDomInfo core
    let n = assignDFNum core info 0 [(0, -1)]
    for i = n downto 1 do
      let v = info.Vertex[i]
      let p = info.Parent[i]
      core.Preds[v] |> List.map (dfnum info) |> computeSemiDom info i
      info.Bucket[info.Semi[i]] <- Set.add i info.Bucket[info.Semi[i]]
      link info p i (* Link the parent (p) to the forest. *)
      computeDomOrDelay info p
    done
    for i = 1 to n do
      if info.IDom[i] <> info.Semi[i] then
        info.IDom[i] <- info.IDom[info.IDom[i]]
      else ()
    done
    info

  let computeDomTree core info =
    let domTree = Array.create info.MaxLength []
    core.BasicBlocks
    |> Array.iter (fun bbl ->
      let idx = bbl.BBLIdx
      let idom = info.IDom[dfnum info idx]
      domTree[idom] <- idx :: domTree[idom])
    domTree

  let dominatorTree core =
    let info = initDominator core
    let tree = computeDomTree core info
    let tree = Array.sub tree 1 (Array.length tree - 1) // Remove a dummy node
    let tree =
      Array.mapi (fun dfNum vs -> dfNum, vs) tree
      |> Array.fold (fun tree (dfNum, vs) ->
          Map.add info.Vertex[dfNum + 1] vs tree) Map.empty
    info, tree
