public class SummationExecutor {
    public SummationExecutor()
    { }

    public static void executeSummation(SummationFormula form, float i, float n)
    {
        float result = form.doSummation(i,n);
        System.out.println("Result of summation: " + result);
    }

    public static void main(String[] args) {
        System.out.println("Summation formula 1 from 1 to 5:");
        executeSummation(new SummationFormula1(), 1,5);
        System.out.println("Summation formula 2 from 1 to 5:");
        executeSummation(new SummationFormula2(),1,5);
    }
}
