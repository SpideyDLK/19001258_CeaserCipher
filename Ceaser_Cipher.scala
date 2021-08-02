object ceaser extends App{

  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def encrypt(c:Char, key:Int, a:String):Char = {
    val res = a((a.indexOf(c.toUpper)+key) % a.size)
    return res
  }
  def decrypt(c:Char, key:Int, a:String):Char = {
    var ind = a.indexOf(c.toUpper)-key
    if (ind<0) ind = 26-Math.abs(ind) //incase of a negative number
    val res = a(ind)
    return res
  }
  def cipher(algo:(Char,Int,String) => Char, s:String, key:Int, a:String):String = {
    val new_s = s.map(algo(_,key,a))
    return new_s
  }

  val ct = cipher(encrypt,"UCSC",6,alphabet)
  val pt = cipher(decrypt,ct,6,alphabet)

  println("\"UCSC\" Encrypted = "+ct)
  println("\""+ ct + "\"" + " Decrypted = "+pt)
}
