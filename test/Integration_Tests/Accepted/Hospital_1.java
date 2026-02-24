class Patient extends Object {
    string name;
    int identification;
    string emailAddress;
    function Patient(n : string {|p0|}, id : int {|p0|}, email : string {|p0|}) {
        name := n;
        identification := id;
        emailAddress := email;
    }
}

class Hospital extends Object {
    string name;
    function Hospital(nm : string {|p0|} ){
        name := name;
    }

    function sendSurvey(email : string {|Survey|}) -> bool {|p0|} {
        // code
        return true;
    }

    function readFromDB() -> string {|Survey|} {
        // code
        return "bob@bobmail.bob";
    }
}

class Main extends Object {
    Patient bob;
    int bob_id;
    Hospital hospital1;
    function Main(b : int {|p0|}){
        bob := new {|p0|} Patient("bob", 123, "bob@bobmail.bob");
        hospital1 := new {|p0|} Hospital("Hospital 1");
        let bob_id : int {|R0|} := 123 in {
            let emailAddress : string {|Survey|} := hospital1.readFromDB() in {
                hospital1.sendSurvey(emailAddress); // Accepted 
            }
        }
    }
}