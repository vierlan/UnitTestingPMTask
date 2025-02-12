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

        assert(result==2000)
      }
      "the income is above basicRateLimit but below higherRateLimit" in {
        val result: Double = taxCalculator.calculateTax(60000)

        assert(result == 12000)
      }
      "the income is above the higherRateLimit" in {
        val result: Double = taxCalculator.calculateTax(150000)

        assert(result == 49250)
      }
    }
  }

  "formattedCurrentTaxAllowance" should {
    "return a string with the income limit of their current tax band" when {
      "income is below personal allowance" in {
        val result = taxCalculator.formattedCurrentTaxAllowance(5000)

        assert(result == "£10000")
      }
      "income is below basic allowance but higher than personal allowance" in {
        val result = taxCalculator.formattedCurrentTaxAllowance(20000)

        assert(result == "£50000")
      }
      "income is below higher allowance but lower than additional allowance" in {
        val result = taxCalculator.formattedCurrentTaxAllowance(60000)

        assert(result == "£125000")
      }
      "income is above additional allowance" in {
        val result = taxCalculator.formattedCurrentTaxAllowance(150000)

        assert(result == "No Limit")
      }
    }
  }
}
