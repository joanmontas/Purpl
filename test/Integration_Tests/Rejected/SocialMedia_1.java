class User extends Object {
    string name;
    int identification;
    function User(n : string {|p0|}, id : int {|p0|}) {
        name := n;
        identification := id;
    }
}

class SocialMedia extends Object {
    string name;
    function SocialMedia(nm : string {|p0|}){
        name := nm;
    }

    function send_marketing_data(u : User {|Marketing|rho|}) -> bool {|p0|} {
        // send marketing data
        return true;
    }
}

class Main extends Object {
    User user0;
    SocialMedia facebook;
    function Main(){
        user0 := new {|p0|} User("Bob", 123);

        facebook := new {|p0|} SocialMedia("Facebook");

        facebook.send_marketing_data(user0);    // Rejected

    }
}