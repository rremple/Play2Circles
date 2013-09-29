package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._

import views._

import models._

object Splitomatic extends Controller {

  /**
   * Split-o-matic Form definition.
   */
  val splitomaticForm = Form[SplitDiagramParameters](
    mapping(
      "total" -> number(min = 0),
      "a" -> number(min = 0),
      "b" -> number(min = 0),
      "ab" -> number(min = 0)
    )(SplitDiagramParameters.apply)(SplitDiagramParameters.unapply))

  /**
   * Display a form with default values and no diagram.
   */
  def form = Action {
    val defaultParams = SplitDiagramParameters(314, 100, 100, 76)
    Ok(html.splitomatic(splitomaticForm.fill(defaultParams), Right(None)));
  }

  /**
   * Handle form submission, draw diagram if we can.
   */
  def submit = Action { implicit request =>
    splitomaticForm.bindFromRequest.fold(
      errors => BadRequest(html.splitomatic(errors, Left(List("Bad request")))), // e.g., field invalid, like circle size<0
      params => validateAndDisplayDiagram(params) // no form errors
    )
  }

  private def validateAndDisplayDiagram(params: SplitDiagramParameters) = {
    params.errors match {
      case List() => displayDiagram(params) // no validation errors
      case _      => BadRequest(html.splitomatic(splitomaticForm.fill(params), Left(params.errors))) // e.g., intersection > circle size
    }
  }

  private def displayDiagram(params: SplitDiagramParameters) = {
    try {
      Ok(html.splitomatic(splitomaticForm.fill(params), Right(Some(SplitDiagram(params))))) // no calculation errors for new diagram
    } catch {
      case e: RuntimeException =>
        BadRequest(html.splitomatic(splitomaticForm.fill(params), Left(List(e.getMessage)))) // e.g. inconsistent geometry
    }
  }

}