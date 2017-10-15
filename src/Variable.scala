class Variable(override val inValue:Object,override val valType: Type.EnumVal, val varName: String) extends Value(inValue, valType) {
  private val name: String = varName
  
  def getName(): String = name
  override def getValue(): Object = value
  override def getValueType(): Type.EnumVal = valueType
}