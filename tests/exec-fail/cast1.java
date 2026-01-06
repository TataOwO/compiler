class A { A() {} }

class B extends A {}

class Main {
    public static void main(String args[]) {
        B b = (B)new A();
    }
}
