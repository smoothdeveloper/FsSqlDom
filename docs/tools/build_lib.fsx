#r "System.dll"
#r "../../lib/Microsoft.SqlServer.TransactSql.ScriptDom.dll"
// Warning: Bad code


open System
open System.Reflection
open System.Collections.Generic
open Microsoft.SqlServer.TransactSql
let decapitalize(text:string) =
  let map =
    [|'A'..'Z'|]
    |> Array.map (fun up -> up, (string up).ToLower())
    |> dict
  let first = text.[0]
  match map.TryGetValue first with
  | true, v -> string v + text.[1..]
  | false, _ -> text

type TypeHier =
  { typ: Type
    children: Map<string, TypeHier> }
  
  member this.SortedChildren() =
    [| for (KeyValue(k,v)) in this.children do yield v|]
    |> Array.sortBy (fun t -> t.typ.Name)

  member this.RenderStrat() =
    if this.children.Count > 0 then RenderChildren else RenderProps

and RenderStrategy = RenderChildren | RenderProps

let objTp = typeof<System.Object>
let startTp = typeof<ScriptDom.TSqlFragment>

let listTypes(addType) =
  let asm = Assembly.GetAssembly(startTp)
  let hier = { typ = startTp; children = Map.empty }
  Array.fold addType hier (asm.GetTypes())

type Options = 
  {
    useRequireQualifiedAccess : TypeHier -> bool
    useOption : Type -> bool
  }
  static member init =
    { 
      useRequireQualifiedAccess = fun _ -> false
      useOption = fun t ->
        if t = typeof<string> then false else
        
        true
    }
let duFieldName (text: string) =
  let text = decapitalize text 
  match text with
  | "to" | "member" | "function"
  | "namespace" 
    -> text + "_"
  | _ -> text
type CodeGenCtx(sb:Text.StringBuilder, options: Options) = 
  let typeHasChildren     = Dictionary<Type, bool>(8000)
  let propReferencedTypes = HashSet<Type>()
  let topLevelTypes       = HashSet<Type>()
  let missingCases        = HashSet<Type>()

  let cont (s:string) (newline:bool) = 
    sb.Append(s) |> ignore
    if newline then sb.Append('\n') |> ignore
  
  let w fmtstr = Printf.kprintf (fun s -> cont s false) fmtstr
  
  let wl fmtstr = Printf.kprintf (fun s -> cont s true) fmtstr
  
  let doesTypeHaveChildren (typ:Type) = 
    typeHasChildren.ContainsKey(typ) && typeHasChildren.[typ]


  let typesUntilStart (typ: Type) =
    let rec impl (ts: Type list) (typ:Type) =
      if typ = objTp then []
      elif typ = startTp then
        ts
      else
        match typ.BaseType with 
        | null -> 
          printfn "Found Null base for %A" typ 
          [] 
        | x -> impl (typ::ts) x
    impl [] typ 

  let rec addRec (acc: TypeHier) (typs: Type list) =
    match typs with
    | [] -> acc
    | tp::typs ->
      let k = tp.Name //tp.FullName
      match Map.tryFind k acc.children with
      | Some(child) ->
        { acc with children = Map.add k (addRec child typs)  acc.children }
      | None ->
        let child = { typ = tp; children = Map.empty }
        let updChild = addRec child typs
        { acc with children = Map.add k updChild acc.children }

  let addType (acc: TypeHier) (typ: Type) =
    addRec acc (typesUntilStart typ)

  let typeProps (typ:Type) =
    [| for m in typ.GetProperties() do
        if m.PropertyType.IsGenericType then
        
          let itemType = m.PropertyType.GetGenericArguments().[0].Name
          yield m.Name, (sprintf "%s<%s>" (m.PropertyType.Name) itemType) 
        else
          yield m.Name, m.PropertyType.Name |]

  let getProps =
    let startProps = startTp.GetProperties() |> Array.map (fun p -> p.Name) |> Set.ofArray  
    fun (typ:Type) ->
      [|for p in typ.GetProperties() do
          if not <| startProps.Contains(p.Name) && p.GetIndexParameters().Length = 0
            then yield p|]
      |> Array.sortBy (fun p -> p.Name)

  let typePropsTrim (typ:Type) =
    let propsTop = typeProps startTp |> Set.ofSeq
    Set.difference (typeProps typ |> Set.ofSeq) propsTop

  let getTypeDep (typ:Type) =
    let ofTyp (typ:Type) =
      match typ.Namespace with
      | "Microsoft.SqlServer.TransactSql.ScriptDom" -> Some(typ)
      | _ -> None 

    if typ.IsGenericType then
      let genTp = typ.GetGenericTypeDefinition()
      match typ.GetGenericArguments() with
      | [|gentypA|] -> ofTyp gentypA
      | x -> failwithf "Unhandled gen arguments ary: %A" x
    else
      ofTyp typ 

  member x.getDestPropName (typ:Type) =
    if typ.IsGenericType then
      let genTp = typ.GetGenericTypeDefinition()
      match typ.GetGenericArguments() with
      | [|gentypA|] ->
        let cleanName = typ.Name.Substring(0, (typ.Name.Length - 2))
        match cleanName with
        | "IList" ->
          sprintf "(%s) list" (x.getDestTypeName gentypA)
        | "Nullable" ->
          sprintf "(%s) option" (x.getDestTypeName gentypA)
        | _ -> sprintf "%s<%s>" cleanName (gentypA.Name)
      | x -> failwithf "Unhandled gen arguments ary: %A" x
    elif not <| typ.IsGenericType then
      if typ.IsEnum || typ.IsValueType then x.getDestTypeName (typ)
      else 
        if options.useOption typ then
          sprintf "%s option" (x.getDestTypeName typ)
        else
          (x.getDestTypeName typ)
    else
      failwithf "Can't handle type %A" (typ)

  member x.getFromCsMapping (typ:Type) =
    match typ.Namespace with
    | "Microsoft.SqlServer.TransactSql.ScriptDom" ->
      if typ.IsEnum then "(* dont do nthing *)"
      else
        let baseName, tname =
          if missingCases.Contains(typ) then
            typ.Name, typ.Name
          else typ.BaseType.Name, typ.Name
        if doesTypeHaveChildren typ then
          sprintf "|> Seq.map (%s.FromCs) " (typ.Name)
        else
          match getProps typ with
          | [||] ->
            sprintf "|> Seq.map (%s.%s) " baseName tname
          | props ->
            let pms = 
              props
              |> Array.map x.getPropertyAccess_FromCS
              |> String.concat ", "
            sprintf "|> Seq.map (fun src -> %s.%s(%s)) " baseName tname pms
    | _ -> sprintf "(* Is mapping needed? %s *)" typ.Namespace

  member x.getPropertyAccess_FromCS (prop:PropertyInfo) =
    if prop.PropertyType.IsGenericType then
      let genTp = prop.PropertyType.GetGenericTypeDefinition()
      match prop.PropertyType.GetGenericArguments() with
      | [|gentypA|] ->
        let cleanName = prop.PropertyType.Name.Substring(0, (prop.PropertyType.Name.Length - 2))
        match cleanName with
        | "IList" ->
          sprintf "(src.%s %s|> List.ofSeq)" prop.Name (x.getFromCsMapping gentypA)
        | "Nullable" ->
          sprintf "(Option.ofNullable (src.%s))" prop.Name
        | _ -> sprintf "%s<%s>" cleanName (gentypA.Name)
      | x -> failwithf "Unhandled gen arguments ary: %A" x
    else
      let typ = prop.PropertyType
      let baseName, tname =
        if missingCases.Contains(typ) then
          typ.Name, typ.Name
        else typ.BaseType.Name, typ.Name
      match prop.PropertyType.Namespace with
      | "Microsoft.SqlServer.TransactSql.ScriptDom" when not prop.PropertyType.IsEnum && not prop.PropertyType.IsValueType ->
        if doesTypeHaveChildren (prop.PropertyType) then
          if options.useOption prop.PropertyType then
            sprintf "(src.%s |> Option.ofObj |> Option.map (%s.FromCs))" (prop.Name) tname
          else
            sprintf "(src.%s |> (%s.FromCs))" (prop.Name) tname
        else
          if options.useOption prop.PropertyType then
            sprintf "(src.%s |> Option.ofObj |> Option.map (%s.FromCs))" (prop.Name) baseName
          else
            sprintf "(src.%s |> (%s.FromCs))" (prop.Name) baseName
      | _ ->
        if prop.PropertyType.IsValueType then
          sprintf "(src.%s)" (prop.Name)
        else
          if options.useOption prop.PropertyType then
            sprintf "(Option.ofObj (src.%s))" (prop.Name)
          else
            sprintf "(src.%s)" (prop.Name)

  member x.getMapping_ToCs (typ:Type) =
    match typ.Namespace with
    | "Microsoft.SqlServer.TransactSql.ScriptDom" ->
      if typ.IsEnum then "x"
      else
        let baseName, tname =
          if missingCases.Contains(typ) then
            typ.Name, typ.Name
          else typ.BaseType.Name, typ.Name
        if doesTypeHaveChildren typ then
          sprintf "x.ToCs() "
        else
          "x"
    | _ -> sprintf "x (* Is mapping needed? %s *)" typ.Namespace
  member x.writePropertyMutation_FromFS (propAccess:string) (indent:int) (prop:PropertyInfo) (typ:Type) : unit =
    let indent = String(' ', indent * 2)
    let varname = x.decapitalizePropName prop.Name

    if not prop.CanWrite && not prop.PropertyType.IsGenericType then
      wl "%s // Skipping prop %s - it is Readonly" indent prop.Name
    elif prop.PropertyType.IsGenericType then
      let genTp = prop.PropertyType.GetGenericTypeDefinition()
      match prop.PropertyType.GetGenericArguments() with
      | [|gentypA|] ->
        let cleanName = prop.PropertyType.Name.Substring(0, (prop.PropertyType.Name.Length - 2))
        match cleanName with
        | "IList" ->
          wl "%sfor e in %s do %s.Add (e.ToCs())" indent varname propAccess
        | "Nullable" ->
          wl "%s%s <- Option.toNullable %s" indent propAccess varname
        | _ -> 
          wl "%s // what is this?" indent
      | x -> failwithf "Unhandled gen arguments ary: %A" x
    else
      let typ = prop.PropertyType
      let baseName, tname =
        if missingCases.Contains(typ) then
          typ.Name, typ.Name
        else typ.BaseType.Name, typ.Name
      match prop.PropertyType.Namespace with
      | "Microsoft.SqlServer.TransactSql.ScriptDom" when not prop.PropertyType.IsEnum && not prop.PropertyType.IsValueType ->
        if options.useOption typ then
          wl "%s%s <- %s |> Option.map (fun x -> x.ToCs()) |> Option.toObj" indent propAccess varname
        else
          wl "%s%s <- let x = %s in  (if isNull (box x) then null else x.ToCs())" indent propAccess varname
      | _ ->
        if prop.PropertyType.IsValueType then
          wl "%s%s <- %s" indent propAccess varname
        elif options.useOption typ then
          wl "%s%s <- %s |> Option.toObj" indent propAccess varname
        else
          wl "%s%s <- %s" indent propAccess varname
  member this.printTree (tree:TypeHier) (sb:Text.StringBuilder) (depth: int) =
    let start = String(' ', depth * 2)
    sb.Append(start) |> ignore
    sb.Append(tree.typ.Name) |> ignore
    for name, propName in (typePropsTrim (tree.typ)) do
      sb.Append(" ") |> ignore
      sb.Append(name) |> ignore
      sb.Append(':') |> ignore
      sb.Append(propName) |> ignore
      sb.Append(" ") |> ignore
    sb.Append("\n") |> ignore
    for (KeyValue(k,v)) in tree.children do
      this.printTree v sb (depth + 1)

  member this.getDestTypeName (typ:Type) = 
    if typ.IsEnum then "ScriptDom." + typ.Name
    else
      match typ.Name with 
      | "Boolean" -> "bool"
      | x ->
        if missingCases.Contains(typ) then
          typ.Name //+ "." + typ.Name
        else
          if doesTypeHaveChildren typ then
            typ.Name
          elif propReferencedTypes.Contains(typ) then
            typ.Name
          else
            match typ.Namespace with
            | "Microsoft.SqlServer.TransactSql.Scriptdom" ->
              failwith "Unhandled!"
            | _ -> typ.Name

  member this.listTypes() = listTypes addType

  member  this.renderPropsTS (thier:TypeHier) (asBase:bool) =
    if not asBase && thier.children.Count > 0 then
        w "((%s.FromCs(src)))" thier.typ.Name
      else
        match getProps thier.typ with
        | [||] -> w ""
        | props ->
          w "("
          for i in [0..props.Length - 1] do
            let prop = props.[i]
            w "%s" (this.getPropertyAccess_FromCS prop)
            if i <> props.Length - 1 then w ", "
          w ")"

  member x.renderDUcase (typ:Type) (asName:string option) =
    let tname = if asName.IsSome then asName.Value else typ.Name
    match getProps typ with
    | [||] ->
      w "  | %s " tname
    | props ->
      w "  | %s of " tname

      for i in [0..props.Length - 1] do
        let prop = props.[i]
        w "%s:%s" (duFieldName prop.Name) (x.getDestPropName prop.PropertyType)
        if i <> props.Length - 1 then w " * "

  member x.decapitalizePropName(propName:string) =
    "a" + propName

  member x.renderPropsDestructure(typ:Type) =
    [ for p in getProps typ do
        yield sprintf "%s=%s" (duFieldName p.Name) (x.decapitalizePropName (p.Name)) ]
    |> String.concat "; "

  member x.renderSetProps_FromFSharp (varname:string) (indentLevel:int) (typ:Type) =
    let indent = String(' ', indentLevel * 2)
    for p in getProps typ do
      let propAccess = sprintf "%s.%s" varname p.Name
      x.writePropertyMutation_FromFS propAccess indentLevel p typ
  member x.renderSharedProperties (tree:TypeHier) (targetTyp:Type option) =
    let props = getProps tree.typ
    if props.Length = 0 then () else
    wl "//// shared props %s "tree.typ.Name
    for p in getProps tree.typ do
      match targetTyp with
      | Some targetTyp ->
        wl
          "  member this.%s = let (%s.%s(%s=x)) = this in x"
          p.Name
          tree.typ.Name
          (targetTyp.Name)
          (duFieldName p.Name)
      | None ->
        wl "  member this.%s = " p.Name
        wl "    match this with"
        if not tree.typ.IsAbstract then
          wl 
            "    | Base (%s=%s) -> %s"
            (duFieldName p.Name)
            (duFieldName p.Name)
            (duFieldName p.Name)
        for c in tree.children do
          let c = c.Value
          if c.typ.IsAbstract || c.children.Count > 0 then 
            wl 
              "    | %s _ as x -> x.%s" 
              c.typ.Name
              p.Name
          else
            wl 
              "    | %s(%s=%s) -> %s"
              c.typ.Name
              (duFieldName p.Name)
              (duFieldName p.Name)
              (duFieldName p.Name)
  member x.renderConstructCSType (tree:TypeHier) (targetTyp:Type) =
    match x.renderPropsDestructure tree.typ with
    | "" ->
      let upcastStr = if tree.typ = targetTyp then "" else sprintf " :> ScriptDom.%s" targetTyp.Name
      wl "    | %s -> ScriptDom.%s()%s (* %s *)" tree.typ.Name tree.typ.Name upcastStr __LINE__
    | s -> 
      wl "    | %s(%s) ->" tree.typ.Name s
      wl "      let ret = ScriptDom.%s()" (tree.typ.Name)
      x.renderSetProps_FromFSharp "ret" 3 tree.typ
      if tree.typ = targetTyp then
        wl "      ret (* %s *)" __LINE__
      else
        wl "      ret :> ScriptDom.%s (* %s *)" targetTyp.Name __LINE__
  member x.renderToCSMethod (tree:TypeHier) (targetTyp:Type) =
    w "  member this.ToCs() : ScriptDom.%s =\n" (tree.typ.Name)
  
    match tree.RenderStrat() with
    | RenderChildren ->
      w "    match this with\n"
      for cctyp in tree.SortedChildren() do
        match cctyp.RenderStrat() with
        | RenderChildren ->
          w "    | %s(x) -> x.ToCs() :> ScriptDom.%s (* %s *)\n" cctyp.typ.Name tree.typ.Name __LINE__
        | RenderProps ->
          x.renderConstructCSType cctyp tree.typ

      if not <| tree.typ.IsAbstract then
        w "    | Base(%s) ->\n\n" (x.renderPropsDestructure tree.typ)  
        w "      let ret = ScriptDom.%s()\n" (tree.typ.Name)
        x.renderSetProps_FromFSharp "ret" 3 tree.typ
        w "      ret\n"

    | RenderProps ->
      w "    match this with\n"
      x.renderConstructCSType tree tree.typ

  member x.printDUs (tree:TypeHier) (sb:Text.StringBuilder) =
    let cont (s:string) = sb.Append(s) |> ignore
  
    let q = Queue()
    let written = HashSet()
    let mutable duNum = 0

    q.Enqueue(tree)

    while q.Count > 0 do
      let tree = q.Dequeue()
    
      if duNum = 0 then w "type " else w "and "
      
      if options.useRequireQualifiedAccess tree then
        w "[<RequireQualifiedAccess>] "
      w "%s = (* IsAbstract = %b , children = %i*)\n" (tree.typ.Name) (tree.typ.IsAbstract) tree.children.Count

      let ctyps = tree.SortedChildren()

      if ctyps.Length > 0 then    
        if not <| tree.typ.IsAbstract then
          x.renderDUcase (tree.typ) (Some("Base"))
          w "\n"
        
        for ctyp in ctyps do
          if ctyp.children.Count > 0 then
            w "  | %s of %s:%s" (ctyp.typ.Name) (duFieldName ctyp.typ.Name)(ctyp.typ.Name)
          else
            x.renderDUcase ctyp.typ None
      
          w "\n"

        x.renderToCSMethod tree tree.typ
        x.renderSharedProperties tree None
        w "  static member FromCs(src:ScriptDom.%s) : %s =\n" (tree.typ.Name) (tree.typ.Name)
        w "    match src with\n"
        for ctyp in ctyps do
          w "    | :? ScriptDom.%s as src ->\n" ctyp.typ.Name
          if ctyp.RenderStrat() = RenderChildren then
            w "      match src with\n"
            let cctyps = ctyp.SortedChildren()
            for cctyp in cctyps do
              w "      | :? ScriptDom.%s as src->\n" cctyp.typ.Name
              w "        %s.%s((%s.%s" tree.typ.Name ctyp.typ.Name ctyp.typ.Name cctyp.typ.Name
              x.renderPropsTS cctyp false
              w "))\n"

            if not <| ctyp.typ.IsAbstract then
              w "      | _ -> (* :? ScriptDom.%s as src *)\n" ctyp.typ.Name
              w "        %s.%s((%s.Base" tree.typ.Name ctyp.typ.Name ctyp.typ.Name 
              x.renderPropsTS ctyp true
              w "))\n"
          else
            w "      %s.%s" (tree.typ.Name) (ctyp.typ.Name)
            match getProps (ctyp.typ) with
            | [||] -> w "\n"
            | cprops ->
              w "("
              for i in [0..cprops.Length - 1] do
                let cprop = cprops.[i]
                w "%s" (x.getPropertyAccess_FromCS cprop)
                if i <> cprops.Length - 1 then w ", "
              w ")\n"
        if not <| tree.typ.IsAbstract then
          w "    | _ -> (* :? ScriptDom.%s as src *)\n" tree.typ.Name
          w "      %s.Base(" tree.typ.Name
          x.renderPropsTS tree true
          w ")\n"
      else ()

      written.Add(tree) |> ignore

      duNum <- duNum + 1
      for (KeyValue(_, subtree)) in tree.children do
        if not <| written.Contains(subtree) && doesTypeHaveChildren (subtree.typ) then 
          q.Enqueue(subtree)
    wl "// Rendering missing cases"
    for typ in missingCases do
      if typ.IsEnum then () else
      let tree = { TypeHier.children = Map.empty; typ = typ }
      //if doesTypeHaveChildren typ then failwith "logic error"
      let recordTypesStr = 
        [| for prop in getProps typ do
            yield sprintf "%s:%s" (duFieldName prop.Name) (x.getDestPropName prop.PropertyType) |]
        |> String.concat " * "
        |> fun s -> if s <> "" then "of " + s else s
      if typ.Name = "WindowFrameType" then 
        printfn "ffoo"
      w "and "  
      if options.useRequireQualifiedAccess tree then
        w "[<RequireQualifiedAccess>] "
      w "%s = (* IsAbstract = %b , children = %i*)\n  | %s %s  \n" typ.Name typ.IsAbstract tree.children.Count typ.Name recordTypesStr 
      let upcastStr = ""
      //renderProps tree.typ
      //if not (typ.IsAbstract) then 
      x.renderToCSMethod tree typ
      x.renderSharedProperties tree (Some typ)
      w "  static member FromCs(src:ScriptDom.%s) : %s =\n" (typ.Name) (typ.Name)

      match getProps typ with
      | [||] ->
        w "    %s.%s %s\n"  (typ.Name) (typ.Name) upcastStr
      | props ->
        w "    %s.%s(" (typ.Name) (typ.Name)

        for i in [0..props.Length - 1] do
          let prop = props.[i]
          w "%s" (x.getPropertyAccess_FromCS prop)
          if i <> props.Length - 1 then w ", "
        w ")%s\n" upcastStr
    ()

  member this.processTree (tree:TypeHier) =
    let rec impl (tree:TypeHier) (depth: int) =
      let typHasChildren = tree.children.Count > 0
    
      if typHasChildren || false (* depth <= 1 *) then topLevelTypes.Add(tree.typ) |> ignore
      //if depth % 2 = 0 then topLevelTypes.Add(tree.typ) |> ignore
      if tree.typ <> startTp then
        for prop in getProps (tree.typ) do
          match getTypeDep (prop.PropertyType) with
          | Some(typ) -> 
            propReferencedTypes.Add(typ) |> ignore 
          | None -> ()

      typeHasChildren.[tree.typ] <- typHasChildren
      for (KeyValue(_, c)) in tree.children do impl c (depth + 1)
    impl tree 0

    for typ in propReferencedTypes do
      if not <| topLevelTypes.Contains(typ) then
        missingCases.Add(typ) |> ignore

let buildLib() = 
  let filePrelude = """module FsSqlDom.Dom
open System
open Microsoft.SqlServer.TransactSql

#nowarn "25"   // Turn off bogus missing pattern match cases warning
#nowarn "1182" // Turn off unused variables warning

"""
  let sb = Text.StringBuilder(filePrelude)

  let ctx = CodeGenCtx(sb, Options.init)
  let tree = ctx.listTypes()
  
  ctx.processTree tree

  ctx.printDUs tree sb
  let outPath = IO.Path.Combine(__SOURCE_DIRECTORY__, "../../src/FsSqlDom/FsSqlDom.fs")
  IO.File.WriteAllText(outPath, sb.ToString())

  let treeSb = Text.StringBuilder()
  ctx.printTree tree treeSb 0

//buildLib()