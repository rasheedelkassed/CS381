import javax.script.ScriptEngineManager;
import javax.script.ScriptEngine;
import javax.script.ScriptException;

public class SummationExecutor {
    public SummationExecutor()
    { }

    public static void executeSummation(SummationFormula form, float i, float n)
    {
        float result = form.doSummation(i,n);
        System.out.println("Result of summation: " + result);
    }

    public static void executeStringSummation(String form, float i, float n) throws ScriptException
    {
        ScriptEngineManager manager = new ScriptEngineManager();
        ScriptEngine engine = manager.getEngineByName("js");
        Double result = 0.0;
        for(; i <= n; i++) {
            engine.put("n", i);
            result += (Double)engine.eval(form);
        }
        System.out.println("Result of summation: " + result);
    }

    public static void main(String[] args) {
        System.out.println("Summation formula 1 from 1 to 5:");
        executeSummation(new SummationFormula1(), 1,5);
        System.out.println("Summation formula 2 from 1 to 5:");
        executeSummation(new SummationFormula2(),1,5);
        System.out.println("Summation formula string from 1 to 5:");
        try {
            executeStringSummation("(n * (n + 1) * ((2 * n) + 1))/6", 1, 5);
        } catch (ScriptException e) {
            System.out.println(e);
        }
    }
}
