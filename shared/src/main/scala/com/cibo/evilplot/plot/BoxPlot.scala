package com.cibo.evilplot.plot

import com.cibo.evilplot.geometry._
import com.cibo.evilplot.numeric.{Bounds, BoxPlotSummaryStatistics}
import com.cibo.evilplot.plot.renderers.{BoxRenderer, PlotRenderer, PointRenderer}

private final case class BoxPlotRenderer(
  data: Seq[BoxPlotSummaryStatistics],
  boxRenderer: BoxRenderer,
  pointRenderer: PointRenderer,
  spacing: Double
) extends PlotRenderer {
  def render(plot: Plot, plotExtent: Extent): Drawable = {
    val xtransformer = plot.xtransform(plot, plotExtent)
    val ytransformer = plot.ytransform(plot, plotExtent)

    data.zipWithIndex.foldLeft(EmptyDrawable(): Drawable) { case (d, (summary, index)) =>
      val x = xtransformer(plot.xbounds.min + index) + spacing / 2
      val y = ytransformer(summary.upperWhisker)

      val boxHeight = ytransformer(summary.lowerWhisker) - ytransformer(summary.upperWhisker)
      val boxWidth = xtransformer(plot.xbounds.min + index + 1) - x - spacing / 2

      val box = boxRenderer.render(plot, Extent(boxWidth, boxHeight), summary)

      val points = summary.outliers.map { pt =>
        pointRenderer.render(plot, plotExtent, index).translate(x = x + boxWidth / 2, y = ytransformer(pt))
      }
      d behind (box.translate(x = x, y = y) behind points.group)
    }
  }
}

object BoxPlot {
  val defaultBoundBuffer: Double = 0.1
  val defaultSpacing: Double = 20

  /** Create a box plots for a sequence of distributions.
    * @param data the distributions to plot
    * @param boxRenderer the `BoxRenderer` to use to display each distribution
    * @param pointRenderer the `PointRenderer` used to display outliers
    * @param quantiles quantiles to use for summary statistics.
    *                  defaults to 1st, 2nd, 3rd quartiles.
    * @param spacing spacing how much spacing to put between boxes
    * @param boundBuffer expand bounds by this factor
    */
  def apply(data: Seq[Seq[Double]],
            boxRenderer: BoxRenderer = BoxRenderer.default(),
            pointRenderer: PointRenderer = PointRenderer.default(),
            quantiles: (Double, Double, Double) = (0.25, 0.50, 0.75),
            spacing: Double = defaultSpacing,
            boundBuffer: Double = defaultBoundBuffer): Plot = {
    val summaries = data.map(dist => BoxPlotSummaryStatistics(dist, quantiles))
    val xbounds = Bounds(0, summaries.size - 1)
    val ybounds = Plot.expandBounds(Bounds(summaries.minBy(_.min).min, summaries.maxBy(_.max).max), boundBuffer)
    Plot(xbounds, ybounds, BoxPlotRenderer(summaries, boxRenderer, pointRenderer, spacing))
  }
}
