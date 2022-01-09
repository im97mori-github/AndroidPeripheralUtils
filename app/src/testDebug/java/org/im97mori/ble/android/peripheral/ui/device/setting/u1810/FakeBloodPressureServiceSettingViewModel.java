package org.im97mori.ble.android.peripheral.ui.device.setting.u1810;

import android.content.Intent;

import androidx.annotation.NonNull;
import androidx.lifecycle.SavedStateHandle;

import com.google.gson.Gson;

import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.subjects.PublishSubject;

@HiltViewModel
public class FakeBloodPressureServiceSettingViewModel extends BloodPressureServiceSettingViewModel {

    public final PublishSubject<String> mObserveSetupSubject = PublishSubject.create();

    public final FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    private final SavedStateHandle mSavedStateHandle;

    public final PublishSubject<Intent> mObserveSaveSubject = PublishSubject.create();

    public java.util.function.Consumer<Boolean> mUpdateIsIntermediateCuffPressureSupportedConsumer;

    @Inject
    FakeBloodPressureServiceSettingViewModel(@NonNull SavedStateHandle savedStateHandle
            , @NonNull FakeDeviceSettingRepository deviceSettingRepository
            , @NonNull Gson gson) {
        super(savedStateHandle, deviceSettingRepository, gson);
        mSavedStateHandle = savedStateHandle;
        mFakeDeviceSettingRepository = deviceSettingRepository;
    }

    @Override
    public void observeSetup(@NonNull Intent intent, @NonNull Action onComplete, @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(mObserveSetupSubject
                .subscribe(s -> mDisposable.add(Single.<String>create(emitter -> emitter.onSuccess(s))
                        .flatMapCompletable(t -> {
                            switch (t) {
                                // @formatter:off
                                case "test_bloodPressureMeasurementCardView_00002" : test_bloodPressureMeasurementCardView_00002(); break;
                                case "test_bloodPressureMeasurementSettingButton_00002" : test_bloodPressureMeasurementSettingButton_00002(); break;
                                case "test_isIntermediateCuffPressureSupported_00001" : test_isIntermediateCuffPressureSupported_00001(); break;
                                case "test_isIntermediateCuffPressureSupported_00002" : test_isIntermediateCuffPressureSupported_00002(); break;
                                case "test_intermediateCuffPressureCardView_visibility_00002" : test_intermediateCuffPressureCardView_visibility_00002(); break;
                                case "test_intermediateCuffPressureCardView_visibility_00003" : test_intermediateCuffPressureCardView_visibility_00003(); break;
                                case "test_intermediateCuffPressureCardView_00002" : test_intermediateCuffPressureCardView_00002(); break;
                                case "test_intermediateCuffPressureSettingButton_00002" : test_intermediateCuffPressureSettingButton_00002(); break;
                                case "test_bloodPressureFeatureCardView_00002" : test_bloodPressureFeatureCardView_00002(); break;
                                case "test_bloodPressureFeatureSettingButton_00002" : test_bloodPressureFeatureSettingButton_00002(); break;
                                case "test_recreate_bloodPressureMeasurementCardView_00002" : test_recreate_bloodPressureMeasurementCardView_00002(); break;
                                case "test_recreate_intermediateCuffPressureCardView_visibility_00001" : test_recreate_intermediateCuffPressureCardView_visibility_00001(); break;
                                case "test_recreate_intermediateCuffPressureCardView_visibility_00002" : test_recreate_intermediateCuffPressureCardView_visibility_00002(); break;
                                case "test_recreate_intermediateCuffPressureCardView_00002" : test_recreate_intermediateCuffPressureCardView_00002(); break;
                                case "test_recreate_bloodPressureFeatureCardView_00002" : test_recreate_bloodPressureFeatureCardView_00002(); break;
                                // @formatter:on
                                default:
                            }
                            return Completable.complete();
                        }).subscribe(onComplete, onError))));
    }

    @Override
    public void updateIsIntermediateCuffPressureSupported(boolean checked) {
        if (mUpdateIsIntermediateCuffPressureSupportedConsumer != null) {
            mUpdateIsIntermediateCuffPressureSupportedConsumer.accept(checked);
        }
        super.updateIsIntermediateCuffPressureSupported(checked);
    }

    @Override
    public void observeSave(@NonNull Consumer<Intent> onSuccess, @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(mObserveSaveSubject.subscribe(onSuccess, onError));
    }

    private void test_bloodPressureMeasurementCardView_00002() {
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_DATA_JSON", "a");
    }

    private void test_bloodPressureMeasurementSettingButton_00002() {
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_DATA_JSON", "a");
    }

    private void test_isIntermediateCuffPressureSupported_00001() {
        mSavedStateHandle.set("KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED", false);
    }

    private void test_isIntermediateCuffPressureSupported_00002() {
        mSavedStateHandle.set("KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED", true);
    }

    private void test_intermediateCuffPressureCardView_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED", false);
    }

    private void test_intermediateCuffPressureCardView_visibility_00003() {
        mSavedStateHandle.set("KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED", true);
    }

    private void test_intermediateCuffPressureCardView_00002() {
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_DATA_JSON", "a");
    }

    private void test_intermediateCuffPressureSettingButton_00002() {
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_DATA_JSON", "a");
    }

    private void test_bloodPressureFeatureCardView_00002() {
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_FEATURE_DATA_JSON", "a");
    }

    private void test_bloodPressureFeatureSettingButton_00002() {
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_FEATURE_DATA_JSON", "a");
    }

    private void test_recreate_bloodPressureMeasurementCardView_00002() {
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_DATA_JSON", "a");
    }

    private void test_recreate_intermediateCuffPressureCardView_visibility_00001() {
        mSavedStateHandle.set("KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED", false);
    }

    private void test_recreate_intermediateCuffPressureCardView_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED", true);
    }

    private void test_recreate_intermediateCuffPressureCardView_00002() {
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_DATA_JSON", "a");
    }

    private void test_recreate_bloodPressureFeatureCardView_00002() {
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_FEATURE_DATA_JSON", "a");
    }

}