class A { A() {} }

class B extends A { }

class C extends B { }

class Main {
    public static void main(String args[]) {
        C c = (C)new A();
    }
}
