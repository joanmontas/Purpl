class Author extends Object {
    string name;
    string last_name;
    int identification;
    function Author(nm : string {|p0|}, lm : string {|p0|}) {
        name := nm;
        last_name := lm;
    }
}

class Article extends Object {
    Author author;
    string title;
    string content;
    function Article(a : Author {|Review|}, t : string {|Review|}, c : string {|Review|}) {
        author := a;
        title := t;
        content := c;
    }
}

class Main extends Object {
    Author a0;
    Author anon_a0;
    Article art_0;
    Article art_1;
    ConferenceManagement cm0;
    function Main(){
        anon_a0 := new {|Review|} Author("foo", "bar");
        anon_a0.grant(Review);
        let anon_title : string {|Review|} := "purpl" in {
            let anon_content : string {|Review|} := "purposes" in {
                art_1 := new {|Review|} Article(anon_a0, anon_title, anon_content); // ACCEPTED
            }
        }
    }
}