public class SummationFormula1 extends SummationFormula {
    public SummationFormula1()
    { }

    public float doSummation(float i, float n)
    {
        float result = 0;
        for(; i <= n; i++)
        {
            result += (i * (i + 1))/2;
        }
        return result;
    }
}
