using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Threading.Tasks;
using VerifyCS = MakeConst.Test.CSharpCodeFixVerifier<
    MakeConst.MakeConstAnalyzer,
    MakeConst.MakeConstCodeFixProvider>;

namespace MakeConst.Test
{
    [TestClass]
    public class MakeConstUnitTest
    {
        [TestMethod]
        public async Task VariableIsAssigned_NoDiagnostic()
        {
            await VerifyCS.VerifyAnalyzerAsync(@"
using System;
class PROGRAM
{
    enum RarityType
    {
        None = 0,
        Normal = 1,
        Rare = 2,
        SuperRare = 3,
        SpecialRare = 4,
        ExtreamRare = 5,
    }

    static void Main(string[] args)
            {
                        // ランダムで生成した数値が0の時出力してreturn
        var random = new Random();
        if(random.Next() == 0)
        {
            Console.WriteLine("");
            return;
        }
        // ランダムで生成した数値が0でないとき
        else
        {
            var first = 0;
        var second = 10;
        var result = first + second;

        Console.WriteLine($"");
        }

    // PIが3.13より大きかったら出力する
    var PI = Math.PI;
        if(PI > 3.13)
        {
            Console.WriteLine("");
        }

// レアリティによって表示を変える
var type = RarityType.None;
switch (type)
{

    case RarityType.None:
        break;
    case RarityType.Rare:
        break;
    case RarityType.SuperRare:
        break;
    case RarityType.SpecialRare:
        break;
}

if (true && true && true && true)
{
if (true && true && true && true)
{
if (true && true && true && true)
{
}
}
}


            }
        }

");
        }
    }
}
