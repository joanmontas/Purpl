// ERROR: checkTerm -> MethodCallTerm  -> checkStateTransition ->
// Final Transition Failed: Purpose R1 ended in TerminatedState but expected ActiveState
class Object {
    string ObjectString;
    function Object(id : string {|p0|}) {
        ObjectString := id;
    }

    // function [R1:active] getObjectID [R1:active] (foo : string {|p0|}) -> int {||} {
    function [R1:active] getObjectID [R1:active] (foo : string {|p0|}) -> int {||} {
        R1.setState(terminated);
        return ObjectID
    }
}

class Main extends Object {
    Object obj;
    string foo;
    function Main(){
        let idd : string {|p0, p1|} := "Hello, World!" in {
            obj := new {|p0, p1|} Object(idd);
            obj.grant(p0)
            let i : string {|p0|} := obj.ObjectString in {
                R1.setState(suspended);
                obj.getObjectID(idd)
            }
        }
    }
}