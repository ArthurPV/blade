open LilyAst.Ast
open LilyAst.Stream

type check_name = {
    ast: ast;
    stream: stream_ast;
    mutable declaration: ast_kind CCVector.vector;
    mutable current_declaration: ast_kind;
}

(*let get_check_declaration sa = 
    let v = CCVector.create () in
    let rec loop n = 
        if n < (CCVector.length sa)-1 then
            match CCVector.get sa n with
            | Expr (ExprVarDefine _)
            | Expr (ExprVarDeclareTypeAndAssign {id = _; 
                                                 tp = _; 
                                                 expr = _})
            | Expr (ExprVarDefineType {id = _;
                                       tp = _})
            | Expr (ExprVarAssign {id = _;
                                   expr = _})
            | Expr (ExprConstDefine _)
            | Expr (ExprVarDeclareTypeAndAssign {id = _; 
                                                 tp = _;
                                                 expr = _})
            | Expr (ExprVarDefineType {id = _;
                                       tp = _})
            | Expr (ExprVarAssign {id = _;
                                   expr = _})
            | Expr (ExprFunDefine {id = _;
                                   tp = _;
                                   ret = _})
            | Expr (ExprTypeDefine _)
            | Expr (ExprTypeDeclare {id = _;
                                     field = _})
            | Expr (ExprDataDeclare {id = _;
                                     args = _;
                                     visibility = _;
                                     field = _;
                                     constructor = _})
            | Expr (ExprExplicitModule {id = _;
                                        body = _})
            | Expr (ExprExplicitClass {id = _;
                                       body = _})
            | Expr (ExprMacroDeclare {id = _;
                                      args = _;
                                      body = _})
            -> (
                CCVector.push v (CCVector.get sa n);
                loop (n+1))
            | _ -> loop (n+1) in
    loop (0);
    v

let new_check_name ast decl sa = {
    ast = ast;
    stream = sa;
    declaration = decl;
    current_declaration = CCVector.get decl 0;
}*)
