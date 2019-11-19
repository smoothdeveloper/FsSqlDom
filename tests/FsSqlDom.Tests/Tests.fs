module FsSqlDom.Tests

// Turn off missing pattern match cases for tests
#nowarn "25"
open FsSqlDom
open FsSqlDom.Dom
open NUnit.Framework

[<Test>]
let ``converting parsed result`` () =
  let sql = "select * from foo"

  match Util.parse(sql) with
  | Success(sqlFrag) ->
    match Util.getQueryExpr(sqlFrag) with
    | Some(QueryExpression.QuerySpecification(_forClause, Some(FromClause.FromClause([tref])), _groupByClause, _havingClause, _offsetClause, 
                                         _orderByClause, _selectElements, _topRowFilter, _uniqueRowFilter, _whereClause)) -> 
        
      match tref with
      | TableReference.TableReferenceWithAlias
          (TableReferenceWithAlias.NamedTableReference
            (schemaObject=Some(SchemaObjectName.Base(baseIdentifier = Some(a))))) ->
          match a with
          | Identifier.Base(value=v) -> 
            Assert.AreEqual(v, "foo")
