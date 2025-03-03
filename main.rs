// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run

seq::seq!(N in 1..4 {
    fn f~N () -> u64 {
        N * 2
    }
});

macro_rules! expand_to_nothing {
    ($arg:literal) => {
        // nothing
    };
}

seq::seq!(N in 0..4 {
    expand_to_nothing!(N);
});

seq::seq!(N in 0..16 {
    #[derive(Copy, Clone, PartialEq, Debug)]
    enum Interrupt {
        #(
            Irq~N,
        )*
    }
});

fn main() {
    let sum = f1() + f2() + f3();
    println!("{sum}");
}
