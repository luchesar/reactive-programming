package suggestions
package gui

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing._
import scala.util.{Try, Success, Failure}
import scala.swing.event._
import swing.Swing._
import javax.swing.UIManager
import Orientation._
import rx.subscriptions.CompositeSubscription
import rx.lang.scala.{Observer, Observable, Subscription}
import observablex._
import search._
import scala.concurrent.duration.Duration
import rx.subjects.PublishSubject

object WikipediaSuggest extends SimpleSwingApplication with ConcreteSwingApi with ConcreteWikipediaApi {

  {
    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
    } catch {
      case t: Throwable =>
    }
  }

  def top = new MainFrame {

    /* gui setup */

    title = "Query Wikipedia"
    minimumSize = new Dimension(900, 600)

    val button = new Button("Get") {
      icon = new javax.swing.ImageIcon(javax.imageio.ImageIO.read(this.getClass.getResourceAsStream("/suggestions/wiki-icon.png")))
    }
    val searchTermField = new TextField
    val suggestionList = new ListView(ListBuffer[String]())
    val status = new Label(" ")
    val editorpane = new EditorPane {

      import javax.swing.border._

      border = new EtchedBorder(EtchedBorder.LOWERED)
      editable = false
      peer.setContentType("text/html")
    }

    contents = new BoxPanel(orientation = Vertical) {
      border = EmptyBorder(top = 5, left = 5, bottom = 5, right = 5)
      contents += new BoxPanel(orientation = Horizontal) {
        contents += new BoxPanel(orientation = Vertical) {
          maximumSize = new Dimension(240, 900)
          border = EmptyBorder(top = 10, left = 10, bottom = 10, right = 10)
          contents += new BoxPanel(orientation = Horizontal) {
            maximumSize = new Dimension(640, 30)
            border = EmptyBorder(top = 5, left = 0, bottom = 5, right = 0)
            contents += searchTermField
          }
          contents += new ScrollPane(suggestionList)
          contents += new BorderPanel {
            maximumSize = new Dimension(640, 30)
            add(button, BorderPanel.Position.Center)
          }
        }
        contents += new ScrollPane(editorpane)
      }
      contents += status
    }

    val eventScheduler = SchedulerEx.SwingEventThreadScheduler

    /**
     * Observables
     * You may find the following methods useful when manipulating GUI elements:
     * `myListView.listData = aList` : sets the content of `myListView` to `aList`
     * `myTextField.text = "react"` : sets the content of `myTextField` to "react"
     * `myListView.selection.items` returns a list of selected items from `myListView`
     * `myEditorPane.text = "act"` : sets the content of `myEditorPane` to "act"
     */

    // TO IMPLEMENT
    val searchTerms: Observable[String] = searchTermField.textValues

    // TO IMPLEMENT
    val suggestions: Observable[Try[List[String]]] = searchTerms.concatRecovered[List[String]](searchTerm => {
      val f = Search.wikipediaSuggestion(searchTerm)
      val s = PublishSubject.create[List[String]]()
      f.onComplete {
        case Success(x) => s.onNext(x); s.onCompleted()
        case Failure(ex) => s.onError(ex); s.onCompleted()
      }
      Observable(s)
    })


    // TO IMPLEMENT
    val suggestionSubscription: Subscription = suggestions.observeOn(eventScheduler) subscribe {
      x => suggestionList.listData = x.get
    }

    // TO IMPLEMENT
    val selections: Observable[String] =
      Observable[String]((observer: Observer[String]) => {
        button.clicks.subscribe(
          x =>{
            val items = suggestionList.selection.items
            if (!items.isEmpty) observer.onNext(items.head)
          },
          ex => observer.onError(ex),
          () => observer.onCompleted()
        )
      })

//      button.clicks.map[String](button =>
//      if(suggestionList.selection.items.isEmpty) ""
//      else suggestionList.selection.items.head
//    )

    // TO IMPLEMENT
    val pages: Observable[Try[String]] = selections.concatRecovered[String](selection => {
      val f = Search.wikipediaPage(selection)
      val s = PublishSubject.create[String]()
      f.onComplete {
        case Success(x) => s.onNext(x); s.onCompleted()
        case Failure(ex) => s.onError(ex); s.onCompleted()
      }
      Observable(s)
    })

    // TO IMPLEMENT
    val pageSubscription: Subscription = pages.observeOn(eventScheduler) subscribe {
      x => editorpane.text = x.get
    }
  }
}


trait ConcreteWikipediaApi extends WikipediaApi {
  def wikipediaSuggestion(term: String) = Search.wikipediaSuggestion(term)

  def wikipediaPage(term: String) = Search.wikipediaPage(term)
}


trait ConcreteSwingApi extends SwingApi {
  type ValueChanged = scala.swing.event.ValueChanged

  object ValueChanged {
    def unapply(x: Event) = x match {
      case vc: ValueChanged => Some(vc.source.asInstanceOf[TextField])
      case _ => None
    }
  }

  type ButtonClicked = scala.swing.event.ButtonClicked

  object ButtonClicked {
    def unapply(x: Event) = x match {
      case bc: ButtonClicked => Some(bc.source.asInstanceOf[Button])
      case _ => None
    }
  }

  type TextField = scala.swing.TextField
  type Button = scala.swing.Button
}
