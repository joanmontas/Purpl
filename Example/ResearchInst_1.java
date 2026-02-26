// Type checks
class ResearchInst {
    string ResearchInstID;
    function ResearchInst(id : string {|p0|}) {
        ResearchInstID := id;
    }

    function [R1:active] enroll [R1:active] (foo : string {|p0|}) -> string {||} {
        // R1.setState(terminated);
        return ResearchInstID
    }

    function [R1:suspended] cert_renewed [R1:active] (foo : string {|p0|}) -> string {||} {
        R1.setState(active);
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
                obj.cert_renewed (idd)
                obj.enroll(idd);

            }
        }
    }
}