import Foundation
import Vision
import AppKit
// usage: vocr <image> [x0 y0 x1 y1]  (optional crop, page fractions, top-left origin)
// prints x0\ty0\tx1\ty1\tconf\ttext in page fractions, top-left origin
let a = CommandLine.arguments
let url = URL(fileURLWithPath: a[1])
guard let img = NSImage(contentsOf: url), var cg = img.cgImage(forProposedRect: nil, context: nil, hints: nil) else { exit(1) }
var cx0 = 0.0, cy0 = 0.0, cx1 = 1.0, cy1 = 1.0
if a.count >= 6 {
  cx0 = Double(a[2])!; cy0 = Double(a[3])!; cx1 = Double(a[4])!; cy1 = Double(a[5])!
  let W = Double(cg.width), H = Double(cg.height)
  cg = cg.cropping(to: CGRect(x: cx0 * W, y: cy0 * H, width: (cx1 - cx0) * W, height: (cy1 - cy0) * H))!
}
let req = VNRecognizeTextRequest()
req.recognitionLevel = .accurate
req.usesLanguageCorrection = true
req.recognitionLanguages = ["en-AU", "en-GB", "en-US"]
let h = VNImageRequestHandler(cgImage: cg, options: [:])
try h.perform([req])
let cw = cx1 - cx0, ch = cy1 - cy0
for o in (req.results ?? []) {
  guard let c = o.topCandidates(1).first else { continue }
  let b = o.boundingBox
  print(String(format: "%.4f\t%.4f\t%.4f\t%.4f\t%.2f\t", cx0 + b.minX * cw, cy0 + (1 - b.maxY) * ch, cx0 + b.maxX * cw, cy0 + (1 - b.minY) * ch, c.confidence) + c.string)
}
