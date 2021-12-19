package org.im97mori.ble.android.peripheral.ui.main;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModel;

import org.im97mori.ble.android.peripheral.datasource.DeviceDataSource;
import org.im97mori.ble.android.peripheral.persistence.Device;

import java.util.List;

import javax.inject.Inject;

import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Flowable;

public class MainViewModel extends ViewModel {

    @Inject
    DeviceDataSource mDeviceDataSource;

    @NonNull
    public Flowable<List<Device>> getDeviceList() {
        return mDeviceDataSource.loadDevices();
    }

    @NonNull
    public Completable deleteAllDevices() {
        return mDeviceDataSource.deleteAllDevices();
    }

}