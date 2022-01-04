package org.im97mori.ble.android.peripheral.ui.main;

import androidx.annotation.NonNull;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;
import org.im97mori.ble.android.peripheral.ui.BaseViewModel;

import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class MainViewModel extends BaseViewModel {

    private final DeviceSettingRepository mDeviceSettingRepository;

    @Inject
    public MainViewModel(@NonNull DeviceSettingRepository deviceSettingRepository) {
        mDeviceSettingRepository = deviceSettingRepository;
    }

    public void observeLoadAllDeviceSetting(Consumer<List<DeviceSetting>> onNext, Consumer<Throwable> onError) {
        mDisposable.add(mDeviceSettingRepository.loadAllDeviceSetting()
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(onNext, onError));
    }

    public void observeDeleteAllDeviceSetting(@NonNull Action onComplete, Consumer<Throwable> onError) {
        mDisposable.add(mDeviceSettingRepository.deleteAllDeviceSetting()
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(onComplete, onError));
    }

    @NonNull
    public Map<Integer, Integer> provideDeviceTypeImageResMap() {
        return mDeviceSettingRepository.provideDeviceTypeImageResMap();
    }

}