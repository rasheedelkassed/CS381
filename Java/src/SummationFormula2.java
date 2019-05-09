public class SummationFormula2 extends SummationFormula{
    public SummationFormula2()
    { }

    public float doSummation(float i, float n)
    {
        float result = 0;
        for(; i <= n; i++)
        {
            result += (i * (i + 1) * ((2 * i) + 1))/6;
        }
        return result;
    }
}
