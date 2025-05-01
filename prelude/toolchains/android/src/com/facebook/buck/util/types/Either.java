/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.types;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.google.common.collect.ComparisonChain;
import java.util.Comparator;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;

/** A discriminated union of two parameters that holds a value of either the LEFT or RIGHT type. */
public abstract class Either<LEFT, RIGHT> {

  /** Returns whether the instance holds a left value. */
  public abstract boolean isLeft();

  /** Returns whether the instance holds a right value. */
  public abstract boolean isRight();

  /**
   * Returns the left value.
   *
   * @throws IllegalStateException if this instance does not hold a left value.
   */
  public abstract LEFT getLeft();

  /**
   * Returns the right value.
   *
   * @throws IllegalStateException if this instance does not hold a right value.
   */
  public abstract RIGHT getRight();

  /** Left value or empty otherwise. */
  public abstract Optional<LEFT> getLeftOption();

  /** Right value or empty otherwise. */
  public abstract Optional<RIGHT> getRightOption();

  /** Apply a function based on whether the instance holds a left or right value. */
  public abstract <X> X transform(
      Function<LEFT, X> lhsTransformer, Function<RIGHT, X> rhsTransformer);

  /** Transform left part of either. */
  public abstract <L1> Either<L1, RIGHT> mapLeft(Function<LEFT, L1> f);

  /** Transform left part of either. */
  public abstract <R1> Either<LEFT, R1> mapRight(Function<RIGHT, R1> f);

  public static <LEFT, RIGHT> Either<LEFT, RIGHT> ofLeft(LEFT value) {
    return new Left<>(value);
  }

  public static <LEFT, RIGHT> Either<LEFT, RIGHT> ofRight(RIGHT value) {
    return new Right<>(value);
  }

  // Close this class to subclassing outside of this file. This avoids ambiguities regarding
  // equality checks.
  private Either() {}

  private static final class Left<LEFT, RIGHT> extends Either<LEFT, RIGHT> {

    private final LEFT value;

    private Left(LEFT value) {
      this.value = value;
    }

    @Override
    public boolean isLeft() {
      return true;
    }

    @Override
    public boolean isRight() {
      return false;
    }

    @Override
    public LEFT getLeft() {
      return value;
    }

    @Override
    @JsonIgnore
    public RIGHT getRight() {
      throw new IllegalStateException("Cannot get Right value of a Left either.");
    }

    @Override
    public Optional<LEFT> getLeftOption() {
      return Optional.of(value);
    }

    @Override
    public Optional<RIGHT> getRightOption() {
      return Optional.empty();
    }

    @Override
    public <X> X transform(Function<LEFT, X> lhsTransformer, Function<RIGHT, X> rhsTransformer) {
      return lhsTransformer.apply(value);
    }

    @Override
    public <L1> Either<L1, RIGHT> mapLeft(Function<LEFT, L1> f) {
      return Either.ofLeft(f.apply(value));
    }

    @SuppressWarnings("unchecked")
    @Override
    public <R1> Either<LEFT, R1> mapRight(Function<RIGHT, R1> f) {
      return (Either<LEFT, R1>) this;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) {
        return true;
      }
      if (o == null || getClass() != o.getClass()) {
        return false;
      }
      Left<?, ?> left = (Left<?, ?>) o;
      return Objects.equals(value, left.value);
    }

    @Override
    public int hashCode() {
      // Hash a bit denoting that this is a Left value.
      return Objects.hash(false, value);
    }

    @Override
    public String toString() {
      return "Left{" + value + '}';
    }
  }

  private static final class Right<LEFT, RIGHT> extends Either<LEFT, RIGHT> {

    private final RIGHT value;

    private Right(RIGHT value) {
      this.value = value;
    }

    @Override
    public boolean isLeft() {
      return false;
    }

    @Override
    public boolean isRight() {
      return true;
    }

    @Override
    @JsonIgnore
    public LEFT getLeft() {
      throw new IllegalStateException("Cannot get Left value of a Right either.");
    }

    @Override
    public RIGHT getRight() {
      return value;
    }

    @Override
    public Optional<LEFT> getLeftOption() {
      return Optional.empty();
    }

    @Override
    public Optional<RIGHT> getRightOption() {
      return Optional.of(value);
    }

    @Override
    public <X> X transform(Function<LEFT, X> lhsTransformer, Function<RIGHT, X> rhsTransformer) {
      return rhsTransformer.apply(value);
    }

    @SuppressWarnings("unchecked")
    @Override
    public <L1> Either<L1, RIGHT> mapLeft(Function<LEFT, L1> f) {
      return (Either<L1, RIGHT>) this;
    }

    @Override
    public <R1> Either<LEFT, R1> mapRight(Function<RIGHT, R1> f) {
      return Either.ofRight(f.apply(value));
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) {
        return true;
      }
      if (o == null || getClass() != o.getClass()) {
        return false;
      }
      Right<?, ?> right = (Right<?, ?>) o;
      return Objects.equals(value, right.value);
    }

    @Override
    public int hashCode() {
      return Objects.hash(true, value);
    }

    @Override
    public String toString() {
      return "Right{" + value + '}';
    }
  }

  /**
   * @return a {@link Comparator} used to compare {@link Either}s.
   */
  public static <LEFT, RIGHT> Comparator<Either<LEFT, RIGHT>> comparator(
      Comparator<LEFT> leftComparator, Comparator<RIGHT> rightComparator) {
    return (a, b) ->
        ComparisonChain.start()
            .compare(
                a.isLeft() ? a.getLeft() : null,
                b.isLeft() ? b.getLeft() : null,
                Comparator.nullsLast(leftComparator))
            .compare(
                a.isRight() ? a.getRight() : null,
                b.isRight() ? b.getRight() : null,
                Comparator.nullsLast(rightComparator))
            .result();
  }

  public static <LEFT extends Comparable<LEFT>, RIGHT extends Comparable<RIGHT>>
      Comparator<Either<LEFT, RIGHT>> comparator() {
    return comparator(Comparator.<LEFT>naturalOrder(), Comparator.<RIGHT>naturalOrder());
  }
}
