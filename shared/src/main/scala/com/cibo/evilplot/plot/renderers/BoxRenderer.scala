/*
 * Copyright (c) 2018, CiBO Technologies, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.cibo.evilplot.plot.renderers

import com.cibo.evilplot.colors.{CategoricalColoring, Color}
import com.cibo.evilplot.geometry.{
  Align,
  BorderRect,
  Drawable,
  Extent,
  Line,
  LineDash,
  LineStyle,
  Rect,
  StrokeStyle
}
import com.cibo.evilplot.numeric.BoxPlotSummaryStatistics
import com.cibo.evilplot.plot.aesthetics.{Theme, ThemedValue}
import com.cibo.evilplot.plot.renderers.BoxRenderer.BoxRendererContext
import com.cibo.evilplot.plot.{LegendContext, Plot}

trait BoxRenderer extends PlotElementRenderer[BoxRendererContext] {
  def render(plot: Plot, extent: Extent, summary: BoxRendererContext)(
    implicit theme: Theme): Drawable
  def legendContext(implicit theme: Theme): LegendContext = LegendContext.empty
}

object BoxRenderer {
  final case class BoxRendererContext(
    summaryStatistics: BoxPlotSummaryStatistics,
    index: Int,
    cluster: Int = 0
  )

  private val ThemedFillColor = ThemedValue(Right((t: Theme) => t.colors.fill))
  private val ThemedStrokeColor = ThemedValue(Right((t: Theme) => t.colors.path))
  private val ThemedLineDash = ThemedValue(Right((t: Theme) => t.elements.lineDashStyle))
  private val ThemedStrokeWidth = ThemedValue(Right((t: Theme) => t.elements.strokeWidth))

  def default(
    fillColor: ThemedValue[Color] = ThemedFillColor,
    strokeColor: ThemedValue[Color] = ThemedStrokeColor,
    lineDash: ThemedValue[LineStyle] = ThemedLineDash,
    strokeWidth: ThemedValue[Double] = ThemedStrokeWidth
  ): BoxRenderer = new BoxRenderer {

    def render(
      plot: Plot,
      extent: Extent,
      context: BoxRendererContext
    )(implicit theme: Theme): Drawable = {
      val summary = context.summaryStatistics
      val scale = extent.height / (summary.upperWhisker - summary.lowerWhisker)
      val topWhisker = summary.upperWhisker - summary.upperQuantile
      val uppperToMiddle = summary.upperQuantile - summary.middleQuantile
      val middleToLower = summary.middleQuantile - summary.lowerQuantile
      val bottomWhisker = summary.lowerQuantile - summary.lowerWhisker

      Align
        .center(
          StrokeStyle(Line(scale * topWhisker, strokeWidth), strokeColor)
            .rotated(90),
          BorderRect
            .filled(extent.width, scale * uppperToMiddle)
            .colored(strokeColor)
            .filled(fillColor),
          BorderRect
            .filled(extent.width, scale * middleToLower)
            .colored(strokeColor)
            .filled(fillColor),
          StrokeStyle(Line(scale * bottomWhisker, theme.elements.strokeWidth), strokeColor)
            .rotated(90)
        )
        .reduce(_ above _)
    }
  }

  def tufte(
    fillColor: ThemedValue[Color] = ThemedFillColor,
    strokeColor: ThemedValue[Color] = ThemedStrokeColor,
    lineDash: ThemedValue[LineStyle] = ThemedLineDash,
    strokeWidth: ThemedValue[Double] = ThemedStrokeWidth
  ): BoxRenderer = new BoxRenderer {

    def render(plot: Plot, extent: Extent, context: BoxRenderer.BoxRendererContext)(
      implicit theme: Theme): Drawable = {
      val summary = context.summaryStatistics
      val scale = extent.height / (summary.upperWhisker - summary.lowerWhisker)
      val topWhisker = summary.upperWhisker - summary.upperQuantile
      val uppperToMiddle = summary.upperQuantile - summary.middleQuantile
      val middleToLower = summary.middleQuantile - summary.lowerQuantile
      val bottomWhisker = summary.lowerQuantile - summary.lowerWhisker

      Align
        .center(
          StrokeStyle(Line(scale * topWhisker, strokeWidth / 2), strokeColor)
            .rotated(90)
            .translate(extent.width / 2),
          StrokeStyle(Line(scale * uppperToMiddle, strokeWidth), strokeColor)
            .rotated(90)
            .translate(extent.width / 2),
          StrokeStyle(Line(scale * middleToLower, strokeWidth), strokeColor)
            .rotated(90)
            .translate(extent.width / 2),
          StrokeStyle(Line(scale * bottomWhisker, strokeWidth / 2), strokeColor)
            .rotated(90)
            .translate(extent.width / 2)
        )
        .reduce(_ above _)

    }
  }
  // TODO: CategoricalColoring.themed probably won't work with this
  def colorBy[A: Ordering](
    colorDimension: Seq[A],
    fillColoring: CategoricalColoring[A] = CategoricalColoring.themed[A],
    strokeColor: ThemedValue[Color] = ThemedStrokeColor,
    lineDash: ThemedValue[LineStyle] = ThemedLineDash,
    strokeWidth: ThemedValue[Double] = ThemedStrokeWidth
  ): BoxRenderer = new BoxRenderer {
    private val colorFunc = fillColoring(colorDimension)

    def render(plot: Plot, extent: Extent, context: BoxRendererContext)(
      implicit theme: Theme): Drawable = {
      BoxRenderer
        .default(
          fillColor = colorFunc(colorDimension(context.index)),
          strokeColor = strokeColor,
          lineDash = lineDash,
          strokeWidth = strokeWidth)
        .render(plot, extent, context)
    }

    override def legendContext(implicit theme: Theme): LegendContext = {
      fillColoring.legendContext(colorDimension, legendGlyph = d => Rect(d))
    }
  }
}
