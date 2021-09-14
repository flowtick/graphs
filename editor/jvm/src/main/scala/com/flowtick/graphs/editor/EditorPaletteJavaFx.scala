package com.flowtick.graphs.editor

import cats.effect.IO
import com.flowtick.graphs.editor.feature.PaletteFeature
import scalafx.geometry.Insets
import scalafx.scene.control.{Accordion, TitledPane, Tooltip}
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout._
import scalafx.scene.paint.Color

class EditorPaletteJavaFx(val messageBus: EditorMessageBus, layout: BorderPane)
    extends PaletteFeature {
  val pane = new BorderPane() {
    visible = false
    background = new Background(
      Array(
        new BackgroundFill(Color.LightGray, new CornerRadii(0), Insets.Empty)
      )
    )

    viewOrder_(-10)

    minWidth = 200
  }

  override def toggleView(enabled: Boolean): IO[Boolean] = IO {
    pane.visible = enabled
    enabled
  }

  override def initPalette(model: EditorModel): IO[Unit] = for {
    _ <- IO {
      layout.left = pane

      lazy val fallBackImage = IO(
        new Image(
          getClass.getClassLoader.getResourceAsStream("2B1C_color.png"),
          50,
          50,
          true,
          true
        )
      )

      model.palette.stencilGroups.zipWithIndex.foreach { case (group, index) =>
        val accordion = new Accordion()
        pane.center = accordion

        val groupPane = new TitledPane()
        groupPane.setText(group.title)
        accordion.panes.add(groupPane)

        if (index == 0) {
          accordion.expandedPane = groupPane
        }

        val groupContent = new FlowPane() {
          padding = Insets.apply(10, 10, 10, 10)
          hgap = 10
        }
        groupPane.content = groupContent

        group.items.foreach { stencil =>
          val stencilImage = stencil.image
            .map(imageSpec =>
              ImageLoaderFx
                .registerImage(s"graphs:palette:${stencil.id}", imageSpec)
            )
            .getOrElse(fallBackImage)

          val stencilView = new ImageView {
            image = stencilImage.unsafeRunSync()
            fitWidth = 32
            fitHeight = 32
          }

          val tooltip = new Tooltip {
            text = stencil.title
          }

          Tooltip.install(stencilView, tooltip)

          val stencilGroup = new VBox {
            maxWidth = 50
            maxHeight = 50

            children.add(stencilView)

            onMouseClicked = e => {
              selectPaletteItem(stencil)

              if (e.getClickCount == 2) {
                createFromStencil(stencil)
              }
            }
          }

          groupContent.children.add(stencilGroup)
        }
      }

    }
  } yield ()
}
