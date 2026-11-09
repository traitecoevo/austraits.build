import Foundation
import Vision
import AppKit
// usage: vocrw <image> x0 y0 x1 y1   (crop, page fractions, top-left origin)
// prints one row per word: line_no\tx0\ty0\tx1\ty1\tconf\tword  (page fractions, top-left origin)
let a = CommandLine.arguments
let url = URL(fileURLWithPath: a[1])
guard let img = NSImage(contentsOf: url), var cg = img.cgImage(forProposedRect: nil, context: nil, hints: nil) else { exit(1) }
let cx0 = Double(a[2])!, cy0 = Double(a[3])!, cx1 = Double(a[4])!, cy1 = Double(a[5])!
let W = Double(cg.width), H = Double(cg.height)
cg = cg.cropping(to: CGRect(x: cx0 * W, y: cy0 * H, width: (cx1 - cx0) * W, height: (cy1 - cy0) * H))!
let req = VNRecognizeTextRequest()
req.recognitionLevel = .accurate
req.usesLanguageCorrection = true
req.recognitionLanguages = ["en-AU", "en-GB", "en-US"]
try VNImageRequestHandler(cgImage: cg, options: [:]).perform([req])
let cw = cx1 - cx0, ch = cy1 - cy0
var ln = 0
for o in (req.results ?? []) {
  guard let c = o.topCandidates(1).first else { continue }
  ln += 1
  let s = c.string
  s.enumerateSubstrings(in: s.startIndex..<s.endIndex, options: .byWords) { _, _, _, _ in }
  // words = runs of non-space characters
  var i = s.startIndex
  while i < s.endIndex {
    while i < s.endIndex && s[i] == " " { i = s.index(after: i) }
    if i >= s.endIndex { break }
    var j = i
    while j < s.endIndex && s[j] != " " { j = s.index(after: j) }
    if let r = try? c.boundingBox(for: i..<j) {
      let b = r.boundingBox
      print(String(format: "%d\t%.4f\t%.4f\t%.4f\t%.4f\t%.2f\t", ln, cx0 + b.minX * cw, cy0 + (1 - b.maxY) * ch, cx0 + b.maxX * cw, cy0 + (1 - b.minY) * ch, c.confidence) + String(s[i..<j]))
    }
    i = j
  }
}
