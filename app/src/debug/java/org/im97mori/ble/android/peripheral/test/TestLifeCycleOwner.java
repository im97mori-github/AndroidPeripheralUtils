package org.im97mori.ble.android.peripheral.test;

import androidx.annotation.NonNull;
import androidx.lifecycle.Lifecycle;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.LifecycleRegistry;

public class TestLifeCycleOwner implements LifecycleOwner {
    @NonNull
    @Override
    public Lifecycle getLifecycle() {
        LifecycleRegistry lifecycleRegistry = new LifecycleRegistry(this);
        lifecycleRegistry.setCurrentState(Lifecycle.State.STARTED);
        return lifecycleRegistry;
    }
}
