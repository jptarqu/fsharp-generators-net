namespace FsGenerator

//TODO this must be availbale to apps too
type PrimitiveTypes =
    | String
    | Integer
    | Decimal
    | Date
    | DateTime
    | Binary

type CommonDataRequirements =
    {Size: int; Precision: int; PrimitiveType:PrimitiveTypes;  }
module SqlTableGenerator = 
    open System.Reflection
    open Humanizer

    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then
            Some(s.Substring(p.Length))
        else
            None
    let private getAllProps<'t> () =
        let mytype = typeof< 't >
        let props = mytype.GetProperties()
        props
    let GetAllPrimitives (props:PropertyInfo array) =
        props 
        |> Seq.filter (fun p -> p.PropertyType.IsPrimitive)
        
    let GetCommonDataRequirementsMethod (mytype:System.Type) =
        mytype.GetMethods(BindingFlags.Static)
        |> Seq.filter (fun m -> m.Name = "GetCommonDataRequirements")
    let isCustomPrimitiveProp (p:PropertyInfo) =
        p.PropertyType |> GetCommonDataRequirementsMethod |> Seq.isEmpty |> not 
    let GetCommonDataRequirements (p:PropertyInfo) =
        let propType = p.PropertyType
        let methodInfo = propType.GetMethods(BindingFlags.Static)
                        |> Seq.filter (fun m -> m.Name = "GetCommonDataRequirements")
                        |> Seq.tryHead
        match methodInfo with
        | None -> None
        | Some met -> Some (met.Invoke(propType, null) :?> CommonDataRequirements)
    let GetAllCustomPrimitives (props:PropertyInfo array) =
        props 
        |> Seq.filter isCustomPrimitiveProp
    let pluralize (str:string) =
        str.Pluralize()
    let convertPrimtiveToSqlType (propType:System.Type) =
        match propType.Name with
        | Prefix "Int" rest -> "int"
        | Prefix "Decimal" rest -> "numeric(14,4)"
        | Prefix "Date" rest -> "DateTime"
        | Prefix "String" rest -> "varchar(60)"
        | _ -> "varchar(255)"
    let convertToSqlType dataReqs =
        match dataReqs.PrimitiveType with
        | String -> "varchar(" +  dataReqs.Size.ToString() + ")"
        | Integer -> "int"
        | Decimal -> "numeric(" +  dataReqs.Size.ToString() + ", " + dataReqs.Precision.ToString() + ")"
        | Date -> "Date"
        | DateTime -> "DateTime"
        | Binary -> "varbinary(max)"
    let buildSqlColumn p =
        let customPrimitive = GetCommonDataRequirements p
        let sqlType = match customPrimitive with
                        | Some dataReqs -> convertToSqlType(dataReqs) 
                        | None -> convertPrimtiveToSqlType(p)
        p.Name + " " + sqlType + " NOT NULL"
    let SqlTable<'t> (primaryKeyNames:string seq) = 
        let mytype = typeof< 't >
        let props = mytype.GetProperties()
        let primtiveProps = props |> GetAllPrimitives
        let customPrimtiveProps = props |> GetAllCustomPrimitives
        let complexProps = props |> Seq.except primtiveProps |> Seq.except customPrimtiveProps
        let header = "CREATE TABLE " + (pluralize mytype.Name)
        let cols = props
                    |> Seq.filter (fun p -> (primtiveProps |> Seq.contains p) || (customPrimtiveProps |> Seq.contains p))
                    |> Seq.map (fun p -> buildSqlColumn p )
        "" + header + "\n(\n" + (String.concat "," cols)