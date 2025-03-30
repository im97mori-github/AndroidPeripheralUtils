package org.im97mori.ble.android.peripheral.utils;

import androidx.activity.ComponentActivity;
import androidx.annotation.NonNull;
import androidx.lifecycle.LifecycleEventObserver;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.ViewModel;
import androidx.lifecycle.ViewModelProvider;
import org.im97mori.ble.android.peripheral.ui.BaseViewModel;

import static androidx.lifecycle.Lifecycle.Event.ON_STOP;

public class AutoDisposeViewModelProvider extends ViewModelProvider {

    private final LifecycleOwner mLifecycleOwner;

    public AutoDisposeViewModelProvider(@NonNull ComponentActivity owner, @NonNull ViewModelProvider.Factory factory) {
        super(owner, factory);
        mLifecycleOwner = owner;
    }

    @NonNull
    @Override
    public <T extends ViewModel> T get(@NonNull Class<T> modelClass) {
        T viewModel = super.get(modelClass);
        if (viewModel instanceof BaseViewModel) {
            mLifecycleOwner.getLifecycle().addObserver((LifecycleEventObserver) (source, event) -> {
                if (ON_STOP == event) {
                    ((BaseViewModel) viewModel).dispose();
                }
            });
        }
        return viewModel;
    }
}
