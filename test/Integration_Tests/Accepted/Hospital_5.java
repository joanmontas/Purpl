class Patient extends Object {
    string name;
    function Patient(n : string {|p0|}) {
        name := n;
    }
}

class Hospital extends Object {
    string name;
    function Hospital(n : string {|p0|}) {
        name := n;
    }

    function enroll_patient(id : int {|R1, PressRelease|rho|}) -> bool{|p0|} {
        this.display_id(id); // Accepted! id contains all the necessary purposes
        return true;
    }

    function display_id(id : int {|PressRelease|rho|}) -> bool {|p0|} {
        // sends data to be released
        return true;
    }
}

class Main extends Object {
    Patient bob;
    int bob_id;
    Hospital hospital1;
    function Main(){
        hospital1 := new {|p0|} Hospital("Hospital 1");
        let bob_id : int {|R1, PressRelease|} := 123 in {
            hospital1.enroll_patient(bob_id);
        }

    }
}