import com.sun.tools.javac.util.Assert.error

class TaxCalculator {
  //lananh
  // Tax bands
  private val personalAllowance: Int = 12570
  private val basicRateLimit: Int = 50270
  private val higherRateLimit: Int = 125140

  // Tax rates
  private val personalAllowanceRate: Double = 0
  private val basicRate: Double = 0.2
  private val higherRate: Double = 0.4
  private val additionalRate: Double = 0.45

  // Capital gains figures
  private val CGTAllowance: Double = 3000
  private val CGTLowerRate: Double = 18
  private val CGTHigherRate: Double = 24
  private val CGTThreshold: Double = 37700

  // Max Band Rates
  private val basicMaxBand = (basicRateLimit - personalAllowance) * basicRate
  private val basicMaxBandNoAllowance = (basicRateLimit) * basicRate
  private val higherMaxBand = (higherRateLimit - basicRateLimit) * higherRate

  // A method to calculate the total amount of tax to be paid, returned as a double
  def calculateTax(income: Double): Double = {
    val band = formattedCurrentTaxAllowance(income)
    if (band == s"£$personalAllowance") {
      0
    } else if (band == s"£$basicRateLimit") {
      val taxToPay: Double = (income - personalAllowance) * basicRate
      taxToPay
    } else if (band == s"£$higherRateLimit") {
      val taxPaidAtHigherRate: Double = (income - basicRateLimit) * higherRate
      basicMaxBand + taxPaidAtHigherRate
    } else {
      val taxPaidAtAdditionalRate = (income - higherRateLimit) * additionalRate
      basicMaxBandNoAllowance + higherMaxBand + taxPaidAtAdditionalRate
    }
  }

  // A method which can tell you if someone is a higher rate taxpayer
  def isHigherRateTaxpayer(income: Double): Boolean = {
    if (income > 50000) {
      true
    } else {
      false
    }
  }

  // A method that will return a string with the income limit of their current tax band.
  // The return will also be formatted, E.g: "£12,500" or "No limit"
  def formattedCurrentTaxAllowance(income: Double): String = {
    if (income <= personalAllowance) {
      s"£$personalAllowance"
    } else if (income <= basicRateLimit) {
      s"£$basicRateLimit"
    } else if (income <= higherRateLimit) {
      s"£$higherRateLimit"
    } else if (income > higherRateLimit) {
      "No Limit"
    } else {
      "error"
    }
  }

def calculatedCapitalGains(capitalGainsIncome: Double, salary: Double): Double = {
    val CGAfterAllowance = capitalGainsIncome - CGTAllowance
    val salaryAfterAllowance = if (salary <= personalAllowance) {
      0
    } else {
      salary - personalAllowance
    }
    val taxableIncome: Double = CGAfterAllowance + salaryAfterAllowance
    if (capitalGainsIncome < CGTAllowance) {
      0
    } else if ( salaryAfterAllowance > CGTThreshold) {
      val CGTPayable = (CGAfterAllowance * CGTHigherRate )/100
      CGTPayable
    } else if (taxableIncome <= CGTThreshold) {
      val CGTTaxPayable = (CGAfterAllowance * CGTLowerRate)/100
      CGTTaxPayable
    } else {
      //val amountAtLowerRate = taxableIncome - CGTThreshold
      val amountAtHigherRate = taxableIncome - CGTThreshold
      val amountAtLowerRate = (taxableIncome - amountAtHigherRate) - salaryAfterAllowance
      val CGTPayableAtLowerRate = ( amountAtLowerRate * CGTLowerRate)/100
      val CGTPayableAtHigherRate = (amountAtHigherRate * CGTHigherRate)/100
      CGTPayableAtLowerRate + CGTPayableAtHigherRate
    }
}

  def calculateBothTaxes(capitalGainsIncome: Double, salary: Double): Double =  {
    val incomeTax: Double = calculateTax(salary)
    val capitalGainsTax: Double = calculatedCapitalGains(capitalGainsIncome, salary)
    incomeTax + capitalGainsTax
  }
}
