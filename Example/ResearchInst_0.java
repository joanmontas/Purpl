// Type Rejected! :(
// ERROR: checkTerm -> MethodCallTerm  -> checkStateTransition ->
// Final Transition Failed: Purpose R1 ended in SuspendedState but expected ActiveState
class ResearchInst {
    string ResearchInstID;
    function ResearchInst(id : string {|p0|}) {
        ResearchInstID := id;
    }

    function [R1:active] enroll [R1:active] (foo : string {|p0|}) -> string {||} {
        // R1.setState(terminated);
        return ResearchInstID
    }
}

class Main extends Object {
    ResearchInst obj;
    string foo;
    function Main(){
        let idd : string {|p0, p1|} := "Hello, World!" in {
            obj := new {|p0, p1|} ResearchInst(idd);
            obj.grant(p0)
            let i : string {|p0|} := obj.ResearchInstID in {
                R1.setState(suspended);
                obj.enroll(idd);
            }
        }
    }
}