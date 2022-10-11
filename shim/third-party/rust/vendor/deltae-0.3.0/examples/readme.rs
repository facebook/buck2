use std::error::Error;
use deltae::*;

fn main() -> Result<(), Box<dyn Error>>{
    // Lab from a string
    let lab0: LabValue = "89.73, 1.88, -6.96".parse()?;
    // Lab directly from values
    let lab1 = LabValue {
        l: 95.08,
        a: -0.17,
        b: -10.81,
    }.validate()?; // Validate that the values are in range

    // Create your own Lab type
    #[derive(Clone, Copy)]
    struct MyLab(f32, f32, f32);

    // Types that implement Into<LabValue> also implement the Delta trait
    impl From<MyLab> for LabValue {
        fn from(mylab: MyLab) -> Self {
            LabValue { l: mylab.0, a: mylab.1, b: mylab.2 }
        }
    }
    let mylab = MyLab(95.08, -0.17, -10.81);

    // Implement DeltaEq for your own types
    impl<D: Delta + Copy> DeltaEq<D> for MyLab {}

    // Assert that colors are equivalent within a tolerance
    assert_delta_eq!(mylab, lab1, DE2000, 0.0, "mylab is not equal to lab1!");

    // Calculate DeltaE between two lab values
    let de0 = DeltaE::new(&lab0, &lab1, DE2000);
    // Use the Delta trait
    let de1 = lab0.delta(lab1, DE2000);
    assert_eq!(de0, de1);

    // Convert to other color types
    let lch0 = LchValue::from(lab0);
    let xyz0 = XyzValue::from(lab1);
    // If DE2000 is less than 1.0, the colors are considered equivalent
    assert!(lch0.delta_eq(&lab0, DE2000, 1.0));
    assert!(xyz0.delta_eq(&lab1, DE2000, 1.0));

    // Calculate DeltaE between different color types
    let de2 = lch0.delta(xyz0, DE2000);
    assert_eq!(de2.round_to(4), de0.round_to(4));
    // There is some loss of accuracy in the conversion.
    // Usually rounding to 4 decimal places is more than enough.

    // Recalculate DeltaE with different method
    let de3 = de2.with_method(DE1976);

    println!("{}\n{}\n{}\n{}\n{}\n{}\n{}\n{}\n",
        lab0, // [L:89.73, a:1.88, b:-6.96]
        lab1, // [L:95.08, a:-0.17, b:-10.81]
        lch0, // [L:89.73, c:7.2094383, h:285.11572]
        xyz0, // [X:0.84574246, Y:0.8780792, Z:0.8542397]
        de0,  // 5.316941
        de1,  // 5.316941
        de2,  // 5.316937
        de3,  // 6.902717
    );

    Ok(())
}
