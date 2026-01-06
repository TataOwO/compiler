class A {
    A() {}
    String ok() { return "ok\n"; }
}
class Main {
    public static void main(String args[]) {
        System.out.print(new A().ok());
    }
}
