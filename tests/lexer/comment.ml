open OUnit2
open LilyFront.Token

let test_comments test_ctxt = 
    let filename = "inputs/comments/comments.li" in
    let t = Utils.lexer_test filename in 
    assert_equal (CCVector.get t 0) (Comment CommentOneLine);
    assert_equal (CCVector.get t 2) (Comment CommentMultiLine);
    assert_equal (CCVector.get t 4) (Comment (CommentDoc (" this is a comment ")))
