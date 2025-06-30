// Natural numbers
#[derive(Clone, Debug)]
pub enum Nat {
    Zero,
    Suc(Box<Nat>),
}

// Pairing
#[derive(Clone, Debug)]
pub struct Pair<A, B>(A, B);

pub fn proj1<A: Clone, B>(p: &Pair<A, B>) -> A {
    p.0.clone()
}

pub fn proj2<A, B: Clone>(p: &Pair<A, B>) -> B {
    p.1.clone()
}

// Recursion: natRec
pub fn nat_rec<T, F>(z: T, f: F, n: &Nat) -> T
where
    T: Clone,
    F: Fn(&Nat, T) -> T + Copy,
{
    match n {
        Nat::Zero => z,
        Nat::Suc(m) => {
            let rec = nat_rec(z, f, m);
            f(m, rec)
        }
    }
}

// Addition
pub fn add(a: &Nat, b: &Nat) -> Nat {
    nat_rec(a.clone(), |_, r| Nat::Suc(Box::new(r)), b)
}

// Multiplication
pub fn mul(a: &Nat, b: &Nat) -> Nat {
    nat_rec(Nat::Zero, |_, r| add(&r, a), b)
}

// Factorial
pub fn fact(n: &Nat) -> Nat {
    nat_rec(Nat::Suc(Box::new(Nat::Zero)), |m, r| {
        mul(&Nat::Suc(Box::new(m.clone())), &r)
    }, n)
}

// Fibonacci pair
pub fn fib2(n: &Nat) -> Pair<Nat, Nat> {
    nat_rec(
        Pair(Nat::Zero, Nat::Suc(Box::new(Nat::Zero))),
        |_, r| {
            let a = proj1(&r);
            let b = proj2(&r);
            Pair(b.clone(), add(&a, &b))
        },
        n,
    )
}

// Fibonacci number
pub fn fib(n: &Nat) -> Nat {
    proj1(&fib2(n))
}

// Ackermann
pub fn ack(m: &Nat, n: &Nat) -> Nat {
    type AckFn = Box<dyn Fn(&Nat) -> Nat>;

    fn ack_rec(m: &Nat) -> AckFn {
        match m {
            Nat::Zero => Box::new(|x: &Nat| Nat::Suc(Box::new(x.clone()))),
            Nat::Suc(m1) => {
                let f = ack_rec(m1);
                Box::new(move |y: &Nat| {
                    nat_rec(f(&Nat::Suc(Box::new(Nat::Zero))), |_, r| f(&r), y)
                })
            }
        }
    }

    ack_rec(m)(n)
}

// Interactivity: show (convert to u64)
pub fn show(n: &Nat) -> u64 {
    match n {
        Nat::Zero => 0,
        Nat::Suc(m) => 1 + show(m),
    }
}

// Interactivity: read (from u64 to Nat)
pub fn read(n: u64) -> Nat {
    if n == 0 {
        Nat::Zero
    } else {
        Nat::Suc(Box::new(read(n - 1)))
    }
}
