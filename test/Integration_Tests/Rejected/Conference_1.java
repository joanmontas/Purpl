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
    function Main(){
        a0 := new {|p0|} Author("John", "Doe");
        art_0 := new {|Review|} Article(a0, "purpl", "purposes") // rejects
    }
}
