class Value(val inValue:Object, val valType: Type.EnumVal) {
  val value = inValue
  val valueType = valType
  
  def getValue(): Object = value 
  def getValueType(): Type.EnumVal = valueType
}