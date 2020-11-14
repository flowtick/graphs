package com.flowtick.graphs.editor

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._

trait EditorMessageBus {
  def subscribe(backend: EditorComponent): IO[EditorComponent]
  def notifyEvent(source: EditorComponent, event: EditorEvent): IO[EditorContext]
  def publish(command: EditorCommand): IO[EditorContext]
  def publishAll(commands: Vector[EditorCommand]): IO[Vector[EditorContext]]
}

final case class Notification(source: EditorComponent, event: EditorEvent)
final case class EditorEffect(source: EditorComponent, effect: IO[Unit])

final case class EditorContext(event: EditorEvent,
                               model: EditorModel,
                               notifications: Vector[Notification] = Vector.empty,
                               commands: Vector[EditorCommand] = Vector.empty,
                               effects: Vector[EditorEffect] = Vector.empty) {
  def transform(f: PartialFunction[EditorEvent, EditorContext]): EditorContext =
    if(f.isDefinedAt(event)) f(event) else this

  def transformIO(f: PartialFunction[EditorEvent, IO[EditorContext]]): IO[EditorContext] =
    if(f.isDefinedAt(event)) f(event) else IO.pure(this)

  def effect(source: EditorComponent)(f: PartialFunction[EditorEvent, IO[Unit]]): IO[EditorContext] =
    if (f.isDefinedAt(event)) IO.pure(copy(effects = effects.:+(EditorEffect(source, f(event))))) else IO.pure(this)

  def addError(source: EditorComponent, error: Throwable): EditorContext = copy(effects = effects.:+(EditorEffect(source, IO.raiseError(error))))

  def addNotification(source: EditorComponent, event: EditorEvent): EditorContext = copy(notifications = {
    val notification = Notification(source, event)

    if (!notifications.contains(notification)) {
      notifications.:+(notification)
    } else notifications
  })

  def addCommand(command: EditorCommand): EditorContext =
    copy(commands = commands.:+(command))

  def updateModel(update: EditorModel => EditorModel): EditorContext =
    copy(model = update(model).copy(version = model.version +1))
}

class EditorController(logRef: Ref[IO, List[EditorEvent]],
                       listenersRef: Ref[IO, List[EditorComponent]],
                       initial: EditorGraph,
                       palette: Option[Palette]) extends EditorMessageBus {
  lazy val modelRef: Ref[IO, EditorModel] = Ref.unsafe[IO, EditorModel](EditorModel(
    editorGraph = initial,
    palette = palette.getOrElse(Palette.defaultPalette)
  ))

  override def subscribe(component: EditorComponent): IO[EditorComponent] =
    for {
      _ <- listenersRef.getAndUpdate(component :: _)
      model <- modelRef.get
      _ <- component.init(model)
    } yield component

  override def notifyEvent(source: EditorComponent, event: EditorEvent): IO[EditorContext] =
    listenersRef.get.map(_.filter(_ != source)).flatMap(notifyListeners(event, _))

  private def notifyListeners(event: EditorEvent,
                              listeners: List[EditorComponent]): IO[EditorContext] =
    (for {
      _ <- logRef.getAndUpdate(event :: _)
      graph <- modelRef.get
      context <- listeners.foldLeft(IO.pure(EditorContext(event, graph))) {
        case (current, next) =>
          current
          .flatMap(next.eval)
          .redeemWith(
            error => IO.raiseError(new RuntimeException(s"unable to evaluate event $event in $next", error)),
            IO.pure
          )
      }
      _ <- modelRef.set(context.model)
      _ <- context.effects
        .map(editorEffect => editorEffect.effect.redeemWith(
          error => IO.raiseError(new RuntimeException(s"unable to evaluate effect in ${editorEffect.source}", error)),
          IO.pure
        )).sequence
      _ <-
        context
        .notifications
        .map(notification => notifyEvent(notification.source, notification.event))
        .sequence
      _ <- if (context.commands.nonEmpty) publishAll(context.commands) else IO.unit
    } yield context).redeemWith(
      error => IO {
        println(s"error $error during notify")
        error.printStackTrace()
      } *> IO.raiseError(new RuntimeException(s"error during notify", error)),
      IO.pure
    )

  override def publish(command: EditorCommand): IO[EditorContext] = for {
    listeners <- listenersRef.get
    notifyContext <- notifyListeners(command, listeners)
  } yield notifyContext

  override def publishAll(commands: Vector[EditorCommand]): IO[Vector[EditorContext]] = for {
    listeners <- listenersRef.get
    contexts <- commands.map(notifyListeners(_, listeners)).sequence
  } yield contexts
}
