package org.im97mori.ble.android.peripheral.utils;

import static androidx.lifecycle.Lifecycle.Event.ON_STOP;

import androidx.activity.ComponentActivity;
import androidx.annotation.NonNull;
import androidx.lifecycle.LifecycleEventObserver;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.ViewModel;
import androidx.lifecycle.ViewModelProvider;

import org.im97mori.ble.android.peripheral.ui.BaseViewModel;

public class MockitoViewModelProvider extends ViewModelProvider {

    private final LifecycleOwner mLifecycleOwner;

    public MockitoViewModelProvider(@NonNull ComponentActivity owner) {
        super(owner);
        mLifecycleOwner = owner;
    }

    @NonNull
    @Override
    public <T extends ViewModel> T get(@NonNull Class<T> modelClass) {
        T viewModel = super.get(getViewModelClass(modelClass));
        if (viewModel instanceof BaseViewModel) {
            mLifecycleOwner.getLifecycle().addObserver((LifecycleEventObserver) (source, event) -> {
                if (ON_STOP == event) {
                    ((BaseViewModel) viewModel).dispose();
                }
            });
        }
        return viewModel;
    }

    // for mockito
    public static <T extends ViewModel> Class<? extends T> getViewModelClass(Class<T> clazz) {
        return clazz;
    }

}
