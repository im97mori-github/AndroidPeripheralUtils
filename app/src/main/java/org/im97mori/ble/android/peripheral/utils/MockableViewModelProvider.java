package org.im97mori.ble.android.peripheral.utils;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModel;
import androidx.lifecycle.ViewModelProvider;
import androidx.lifecycle.ViewModelStoreOwner;

public class MockableViewModelProvider extends ViewModelProvider {

    public MockableViewModelProvider(@NonNull ViewModelStoreOwner owner) {
        super(owner);
    }

    @NonNull
    @Override
    public <T extends ViewModel> T get(@NonNull Class<T> modelClass) {
        return super.get(getViewModelClass(modelClass));
    }

    public static  <T extends ViewModel> Class<? extends T> getViewModelClass(Class<T> clazz) {
        return clazz;
    }

}
