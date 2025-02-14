import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class TaxCalculatorSpec extends AnyWordSpec {

  val taxCalculator: TaxCalculator = new TaxCalculator

  // I've done the first test for you!
  "TaxCalculator.calculateTax" should {
    "return the total amount of tax to pay" when {
      "the income is below the personal tax limit" in {
        val result: Double = taxCalculator.calculateTax(5000)

        assert(result == 0)
      }
      "the income is above personal tax limit, but below basic rate limit" in {
        val result: Double = taxCalculator.calculateTax(20000)

        assert(result==1486)
      }
      "the income is above basicRateLimit but below higherRateLimit" in {
        val result: Double = taxCalculator.calculateTax(60000)

        assert(result == 11432)
      }
      "the income is above the higherRateLimit" in {
        val result: Double = taxCalculator.calculateTax(150000)

        assert(result == 51189)
      }
    }
  }

  "formattedCurrentTaxAllowance" should {
    "return a string with the income limit of their current tax band" when {
      "income is below personal allowance" in {
        val result = taxCalculator.formattedCurrentTaxAllowance(5000)

        assert(result == "£12570")
      }
      "income is below basic allowance but higher than personal allowance" in {
        val result = taxCalculator.formattedCurrentTaxAllowance(20000)

        assert(result == "£50270")
      }
      "income is below higher allowance but lower than additional allowance" in {
        val result = taxCalculator.formattedCurrentTaxAllowance(60000)

        assert(result == "£125140")
      }
      "income is above additional allowance" in {
        val result = taxCalculator.formattedCurrentTaxAllowance(150000)

        assert(result == "No Limit")
      }
    }
  }

  "calculatedCapitalGains" should {
    "return calculation of Capital Gains Tax payable" when {
      "combined income is less than threshold" in {
        val result = taxCalculator.calculatedCapitalGains(20000, 30000)

        assert(result == 3060)

      }
      "salary is less than threshold but combined income is more than threshold" in {
        val result = taxCalculator.calculatedCapitalGains(30000, 40000)

        assert(result.ceil == 5864)

      }
      "salary is more than threshold" in {
        val result = taxCalculator.calculatedCapitalGains(30000, 70000)

        assert(result == 6480)

      }
      "capital gains income is less than capital gains allowance" in {
        val result = taxCalculator.calculatedCapitalGains(2900, 70000)

        assert(result == 0)

      }
    }
  }

  "calculateBothTaxes" should {
    "return total taxes payable for combined salary and capital gains income" when {

      "salary is below personal threshold and CGT payable is above CGT threshold" in {
        val result = taxCalculator.calculateBothTaxes(70000, 10000)

        assert(result.ceil == 13818)
      }
    }
  }
}
