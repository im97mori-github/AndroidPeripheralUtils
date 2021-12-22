package org.im97mori.ble.android.peripheral.ui.main;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModel;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceRepository;
import org.im97mori.ble.android.peripheral.room.Device;

import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Flowable;

@HiltViewModel
public final class MainViewModel extends ViewModel {

    private final DeviceRepository mDeviceRepository;

    @Inject
    MainViewModel(@NonNull DeviceRepository deviceRepository) {
        mDeviceRepository = deviceRepository;
    }

    @NonNull
    public Flowable<List<Device>> getDeviceList() {
        return mDeviceRepository.loadDevices();
    }

    @NonNull
    public Completable deleteAllDevices() {
        return mDeviceRepository.deleteAllDevices();
    }

    @NonNull
    public Map<Integer, Integer> provideDeviceTypeImageResMap() {
        return mDeviceRepository.provideDeviceTypeImageResMap();
    }

}