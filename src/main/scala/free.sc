import freestyle._
// import freestyle._

case class User(id: Long, name: String)
// defined class User

@free trait UserRepository {
  def get(id: Long): FS[User]
  def save(user: User): FS[User]
  def list: FS[List[User]]
}
// defined trait UserRepository
// defined object UserRepository

