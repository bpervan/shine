package rise.core.types

// TODO: should not be in the core
sealed trait AddressSpace

final case class AddressSpaceIdentifier(
    name: String,
    override val isExplicit: Boolean = false
) extends AddressSpace
    with Kind.Identifier
    with Kind.Explicitness {
  override def toString: String = if (isExplicit) name else "_" + name
  override def asExplicit: AddressSpaceIdentifier = this.copy(isExplicit = true)
  override def asImplicit: AddressSpaceIdentifier =
    this.copy(isExplicit = false)
}

// scalastyle:off public.methods.have.type
object AddressSpace {
  object Global extends AddressSpace { override def toString = "Global" }

  object Local extends AddressSpace { override def toString = "Local" }

  object Private extends AddressSpace { override def toString = "Private" }

  object Constant extends AddressSpace { override def toString = "Constant" }
}
// scalastyle:off public.methods.have.type
