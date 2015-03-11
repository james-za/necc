package necc.simulation

object Category {
  val none = 0x00
  val default = 0x01
  val agent = 0x02
  val block = 0x04
  val obstacle = 0x08
  val structure = 0x10
  val unloadingAgent = 0x20
  val unloadingBlock = 0x40
  val unloadingSensor = 0x80
  val all = 0xFFFF
}