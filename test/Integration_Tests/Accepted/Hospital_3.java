class Patient extends Object {
    string name;
    int identification;
    function Patient(n : string {|p0|}, id : int {|p0|}) {
        name := n;
        identification := id;
    }
}

class Hospital extends Object {
    string name;
    function Hospital(n : string {|p0|}) {
        name := n;
    }

    function enroll_patient(id : int {|R1|}, std : int {|R1|}) -> bool {|p0|} {
        // Some logic
        return true;
    }
}

class Main extends Object {
    Patient bob;
    int bob_id;
    int bob_std;
    Hospital hospital1;
    function Main(){
        bob := new {|p0|} Patient("bob", 123);
        hospital1 := new {|p0|} Hospital("Hospital 1");
        let bob_id : int {|R1|} := 123 in {
            let bob_obesity : int {|R0|} := 350 in {
                bob_obesity.grant(R1);
                hospital1.enroll_patient(bob_id, bob_obesity) // accepted
            }
        }
    }
}